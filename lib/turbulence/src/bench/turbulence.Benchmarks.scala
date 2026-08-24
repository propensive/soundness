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
package turbulence

import scala.collection.immutable.IndexedSeq

import ambience.*, environments.javaEnvironment, systems.javaSystem
import enigmatic.*, blockCipherMode.cbc, blockCipherPadding.pkcs7
import gastronomy.providers.javaStdlibProvider, gastronomy.crypto.permitUnauthenticatedCrypto
import parasite.*, threading.virtualThreading, probates.panicProbate
import anticipation.*
import contingency.*, strategies.throwUnsafely
import denominative.*
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import hieroglyph.*, charDecoders.utf8Decoder, charEncoders.utf8Encoder,
    textSanitizers.strictSanitizer
import lineSeparation.adaptiveLinefeedLineSeparation
import monotonous.*, alphabets.base64Standard, alphabets.hexLowerCase, alphabets.base32LowerCase
import prepositional.*
import probably.*
import proscenium.*
import quantitative.*
import rudiments.*
import sedentary.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import vacuous.*
import zephyrine.*
import pneumatic.*

// Comparative streaming benchmarks: Soundness's pull `Stream` kernel against the
// effect-based streaming libraries ZIO-Streams, FS2 and Kyo. Each benchmark's
// implementation is written inline in its `bench` block; the shared data corpora
// and the `runZio` / `buffering` run helpers below are the only members the
// staged bodies reference (by fully-qualified name).
//
// The comparison is informative but NOT algorithm-symmetric — read it with these
// architectural differences in mind:
//   * The effect libraries wrap each operation in `IO`/`ZIO`/a Kyo effect that is
//     then executed (`unsafeRunSync` / `Runtime.unsafe.run` / `.eval`); that
//     allocation + fiber/interpreter scheduling is part of the measured cost.
//     Soundness runs synchronously on mutable chunk buffers with no effect
//     wrapper. This is the real-world usage comparison, not a kernel-vs-kernel
//     one.
//   * The checksum fold is element-wise in FS2/ZIO/Kyo (each `Byte` is boxed as
//     it flows through the pipeline), whereas Soundness folds over the raw
//     `Array[Byte]` window with no per-element boxing — the point of that row is
//     precisely to show that per-element cost.
//   * FS2/ZIO wrap the input array without copying (`Chunk.array`/`Chunk.fromArray`);
//     `value.stream` copies once at construction. Kyo is fed an `ArraySeq`
//     wrapper (no copy) but boxes per element.
//   * Kyo has no gzip pipeline and no incremental UTF-8 decoder, so those
//     suites stay without a Kyo row; it appears where its own primitives
//     correspond — the checksum fold, the `Channel` hand-off rows, `Stream`
//     fan-in (`collectAll`) and fan-out (`broadcast3`).

object Benchmarks extends Suite(m"Streaming benchmarks: Soundness vs ZIO / FS2 / Kyo"):
  sealed trait Information extends Dimension
  sealed trait Bytes[Power <: Nat] extends Units[Power, Information]
  val Byte: MetricUnit[Bytes[1]] = MetricUnit(1.0)

  given byteDesignation: Designation[Bytes[1]] = () => t"B"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice
  given prefixes:        Prefixes               = Prefixes(List(Kilo, Mega, Giga, Tera))
  given Buffering                               = Buffering.standard

  // ── Corpora (forced once in warmup; referenced by fully-qualified name in the
  //    staged bodies) ────────────────────────────────────────────────────────

  // 4 MB of semi-compressible bytes: a repeating low-period pattern so gzip has
  // real but not trivial work to do.
  lazy val input: Data = Data.fill(4 << 20)(i => ((i*31 + (i >> 6)) & 0xff).toByte)
  lazy val inputArray: scala.Array[Byte] = input.asInstanceOf[scala.Array[Byte]]
  // Kyo's `Stream.init` wants a stdlib `Seq`; a zero-copy `ArraySeq` keeps the
  // comparison fair, so this deliberately stays a stdlib interop boundary.
  lazy val inputSeq: scala.collection.immutable.ArraySeq[Byte] =
    scala.collection.immutable.ArraySeq.unsafeWrapArray(inputArray)

  // A window onto `input`. `Data` is a frozen `Array[Byte]`, which has no `slice` of its
  // own, so this reads through the read-only view and re-freezes the result.
  def slice(from: Int, until: Int): Data = Array.frozen(input.readable.slice(from, until))
  // The same 4 MB split into 64 KiB chunks, so aggregation/write loops iterate
  // (a single in-memory chunk would let `read[Data]` fold to an identity).
  lazy val inputChunks: Chain[Data] =
    ((0 until input.length by 65536).map: offset =>
      slice(offset, (offset + 65536).min(input.length))).to(Chain)
  // The same chunks as a stdlib `List`. The rival pipelines fold and map over the corpus with
  // their own combinators, which need a stdlib collection; like `inputSeq` above, this is a
  // deliberate interop boundary rather than a gap in `Chain`.
  lazy val inputChunkList: scala.collection.immutable.List[Data] = inputChunks.stdlib.toList
  // The 4 MB split into four equal parts, one per source stream for fan-in.
  lazy val quarters: IndexedSeq[Data] =
    val q = input.length/4
    IndexedSeq.tabulate(4)(i => slice(i*q, if i == 3 then input.length else (i + 1)*q))

  // ~4 MB of UTF-8 text with multi-byte characters, so the decode exercises the
  // cross-chunk continuation path in every library.
  lazy val textData: Data =
    val unit = t"The quick brown fox — jümps over the lazy dog. café ☕ 数据 🚀\n"
    val builder = new java.lang.StringBuilder(4 << 20)
    while builder.length < (4 << 20) do builder.append(unit.s)
    Data(builder.toString.getBytes("UTF-8").nn*)
  lazy val textArray: scala.Array[Byte] = textData.asInstanceOf[scala.Array[Byte]]

  // The text corpus pre-compressed with gzip, for the "read a gzipped text
  // stream" chained pipeline.
  lazy val gzippedText: Data = textData.stream.compress[Gzip].memoize
  lazy val gzippedTextArray: scala.Array[Byte] = gzippedText.asInstanceOf[scala.Array[Byte]]

  // The byte corpus pre-compressed with gzip, for the standalone decompression
  // suite.
  lazy val gzippedInput: Data = input.stream.compress[Gzip].memoize
  lazy val gzippedInputArray: scala.Array[Byte] = gzippedInput.asInstanceOf[scala.Array[Byte]]

  // The same 4 MB pre-compressed with Brotli, for the standalone decompression benchmark.
  lazy val brotliInput: Data = input.stream.compress[Brotli].memoize
  lazy val brotliInputArray: scala.Array[Byte] = brotliInput.asInstanceOf[scala.Array[Byte]]

  // ── Small (256 KiB) per-operation corpora for the saturated stress suites. A 4 MB
  //    pipeline operation runs for tens of milliseconds serially — coarser than the
  //    latency SLO a capacity search holds it to — so the saturated rows work in 256 KiB
  //    units, keeping serial operation latency well under the threshold. ──────────────
  lazy val smallInput: Data = slice(0, 256*1024)
  lazy val smallInputArray: scala.Array[Byte] = smallInput.asInstanceOf[scala.Array[Byte]]
  lazy val smallGzipped: Data = smallInput.stream.compress[Gzip].memoize
  lazy val smallGzippedArray: scala.Array[Byte] = smallGzipped.asInstanceOf[scala.Array[Byte]]

  // Built from whole text units, as `textData` is — not a byte-slice of it, which could
  // cut a multi-byte character at the corpus end.
  lazy val smallText: Data =
    val unit = t"The quick brown fox — jümps over the lazy dog. café ☕ 数据 🚀\n"
    val builder = new java.lang.StringBuilder(256*1024 + 128)
    while builder.length < 256*1024 do builder.append(unit.s)
    Data(builder.toString.getBytes("UTF-8").nn*)
  lazy val smallTextArray: scala.Array[Byte] = smallText.asInstanceOf[scala.Array[Byte]]

  // The 256 KiB split into four equal parts, one per source stream for saturated fan-in.
  lazy val smallQuarters: IndexedSeq[Data] =
    val q = smallInput.length/4
    IndexedSeq.tabulate(4)(i => Array.frozen(smallInput.readable.slice(i*q, (i + 1)*q)))

  // AES-256 key + a fixed key/IV for the JDK reference, generated/derived once.
  lazy val aesKey: SymmetricKey[Aes[256] over Cbc against Pkcs7] =
    import enigmatic.cloaks.cloakHeap
    SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
  lazy val jdkKeyBytes: scala.Array[Byte] = scala.Array.tabulate(32)(i => (i*7 + 1).toByte)
  lazy val jdkIvBytes:  scala.Array[Byte] = scala.Array.tabulate(16)(i => (i*13 + 3).toByte)

  // ── Separator-scan variants (see the "Separator scan" suite) ───────────────
  //
  // Each mimics the byte duct's real call pattern — scan to the next separator,
  // step over it, resume — rather than one pass over the corpus, since entering
  // and leaving the loop once per line is part of what is being measured. All
  // three return the line count, so a variant that mis-detects shows up as a
  // different result.

  // What the duct does today: two comparisons and, because `&&` short-circuits,
  // two branches per byte.
  def scanPairwise(bytes: scala.Array[Byte]): Int =
    var position = 0
    var lines = 0

    while position < bytes.length do
      var index = position

      while index < bytes.length && { val byte = bytes(index); byte != 10 && byte != 13 }
      do index += 1

      lines += 1
      position = index + 1

    lines

  // 10 and 13 share their top five bits, so one mask-and-compare rejects every
  // byte outside 8-15 with a single branch; only that range takes the exact
  // test. Sign extension needs no masking away — the mask clears those bits.
  // Admits tab (9) as a false positive, which is common in real text.
  def scanMasked(bytes: scala.Array[Byte]): Int =
    var position = 0
    var lines = 0

    while position < bytes.length do
      var index = position

      while index < bytes.length
          && { val byte = bytes(index)
               (byte & 0xf8) != 0x08 || (byte != 10 && byte != 13) }
      do index += 1

      lines += 1
      position = index + 1

    lines

  // Adding two maps 10 and 13 onto 12 and 15, which share six bits, so the mask
  // admits only 10-13: one more operation per byte, but no false positive on
  // tab — only on VT and FF, which are vanishingly rare.
  def scanBiased(bytes: scala.Array[Byte]): Int =
    var position = 0
    var lines = 0

    while position < bytes.length do
      var index = position

      while index < bytes.length
          && { val byte = bytes(index) + 2
               (byte & 0xfc) != 0x0c || (byte != 12 && byte != 15) }
      do index += 1

      lines += 1
      position = index + 1

    lines

  // ── Shared run helpers (referenced from the staged bodies) ──────────────────

  // ZIO's unsafe-run entry point, wrapping each ZIO benchmark's effect.
  def runZio[A](effect: zio.ZIO[Any, Throwable, A]): A =
    zio.Unsafe.unsafe: (unsafe: zio.Unsafe) ?=>
      zio.Runtime.default.unsafe.run(effect).getOrThrow()
  // A fixed-capacity `Buffering`, for the block-size sweep.
  def buffering(n: Int): Buffering = new Buffering:
    def capacity(substrate: Substrate): Int = n
    def depth: Int = 4

  // A fixed-capacity, fixed-depth `Buffering`, for the ring-depth sweep. The
  // standard 4096-byte capacity keeps the transfer block at its standard 64 KiB,
  // so only the ring depth varies between rows.
  def buffering(n: Int, depth0: Int): Buffering = new Buffering:
    def capacity(substrate: Substrate): Int = n
    def depth: Int = depth0

  // Deterministic, data-independent CPU work proportional to `count`: the
  // slow-consumer rows charge every rival's consumer the same per-block drag, so
  // the producer runs ahead and what differs between rows is the hand-off's
  // buffering policy. Callers fold the result into theirs (`& 1L`), keeping the
  // loop alive under JIT.
  def burn(count: Int): Long =
    var acc: Long = 0L
    var index: Int = 0

    while index < count*4 do
      acc = acc*31 + index
      index += 1

    acc

  // A fresh copy of a corpus chunk. The slow-consumer rows hand off
  // producer-allocated data, as a socket or file read would: passing the shared
  // corpus by reference would let an unbounded hand-off buffer an arbitrary lead
  // for the cost of its queue nodes alone, hiding exactly the retention the rows
  // exist to measure.
  def freshChunk(chunk: Data): Data =
    java.util.Arrays.copyOf(chunk.asInstanceOf[scala.Array[Byte]], chunk.length).nn
    . asInstanceOf[Data]

  // Int rather than Long: ZIO's `take`/`drop` are `Int`-counted, and both values
  // fit; Soundness's and FS2's `Long`-counted versions widen automatically.
  val dropBytes: Int = 65536
  val takeBytes: Int = 2*1024*1024

  def run(): Unit =
    val bench = Bench()
    val stress = Stress()
    val constrained = Stress(heap = t"128m")
    val gated = Stress(heap = t"2g", cpus = 4)
    val saturated = Stress(heap = t"2g")
    val profile = Profile()
    val size = input.length*Byte
    val textSize = textData.length*Byte

    // Example 1: gzip compression (drain, count output bytes).
    suite(m"Gzip compression (4 MB)"):
      bench(m"Soundness  Stream.compress[Gzip]")
        ( target = 1*Second, operationSize = size ):
        '{ turbulence.Benchmarks.input.stream.compress[Gzip].memoize.length }

      bench(m"FS2  Compression[IO].gzip")(target = 1*Second, operationSize = size):
        '{
            import cats.effect.unsafe.implicits.global
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.inputArray)).covary[cats.effect.IO]
            . through(fs2.compression.Compression.forSync[cats.effect.IO].gzip())
            . compile.count.unsafeRunSync()
        }

      bench(m"ZIO  ZPipeline.gzip")(target = 1*Second, operationSize = size):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.inputArray))
              . via(zio.stream.ZPipeline.gzip())
              . runCount
        }

    // Example 1b: gzip decompression alone (drain, count output bytes) — the
    // inverse of example 1, on the pre-gzipped corpus, so inflate performance
    // is visible unchained. `GZIPInputStream` (64 KiB buffer) is the JDK
    // reference.
    suite(m"Gzip decompression (4 MB)"):
      bench(m"Soundness  Stream.decompress[Gzip]")
        ( target = 1*Second, operationSize = size ):
        '{ turbulence.Benchmarks.gzippedInput.stream.decompress[Gzip].memoize.length }

      bench(m"FS2  Compression[IO].gunzip")(target = 1*Second, operationSize = size):
        '{
            import cats.effect.unsafe.implicits.global
            val comp = fs2.compression.Compression.forSync[cats.effect.IO]
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.gzippedInputArray))
            . covary[cats.effect.IO]
            . through(comp.gunzip()).flatMap(_.content)
            . compile.count.unsafeRunSync()
        }

      bench(m"ZIO  ZPipeline.gunzip")(target = 1*Second, operationSize = size):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.gzippedInputArray))
              . via(zio.stream.ZPipeline.gunzip())
              . runCount
        }

      bench(m"JDK  GZIPInputStream")(target = 1*Second, operationSize = size):
        '{
            val in =
              java.util.zip.GZIPInputStream
                (java.io.ByteArrayInputStream(turbulence.Benchmarks.gzippedInputArray), 65536)

            val buffer = new scala.Array[Byte](65536)
            var total = 0L
            var count = in.read(buffer)

            while count >= 0 do
              total += count
              count = in.read(buffer)

            total
        }

    // Example 1c: Brotli compression (drain, count output bytes). Brotli has no FS2/ZIO pipeline,
    // so the informative comparison is against our own Gzip and the raw output size.
    suite(m"Brotli compression (4 MB)"):
      bench(m"Soundness  Stream.compress[Brotli]")
        ( target = 1*Second, operationSize = size ):
        '{ turbulence.Benchmarks.input.stream.compress[Brotli].memoize.length }

      bench(m"Soundness  Stream.compress[Gzip]")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.input.stream.compress[Gzip].memoize.length }

    // Example 1d: Brotli decompression alone, on the pre-Brotli'd corpus. The reference pure-Java
    // decoder `org.brotli.dec.BrotliInputStream` is the "competitive-with-Java" baseline — our port
    // implements the same algorithm, so this row shows how close the pure-Scala port runs to it.
    suite(m"Brotli decompression (4 MB)"):
      bench(m"Soundness  Stream.decompress[Brotli]")
        ( target = 1*Second, operationSize = size ):
        '{ turbulence.Benchmarks.brotliInput.stream.decompress[Brotli].memoize.length }

      bench(m"Java  org.brotli.dec.BrotliInputStream")(target = 1*Second, operationSize = size):
        '{
            val in =
              org.brotli.dec.BrotliInputStream
                (java.io.ByteArrayInputStream(turbulence.Benchmarks.brotliInputArray), 65536)

            val buffer = new scala.Array[Byte](65536)
            var total = 0L
            var count = in.read(buffer)

            while count >= 0 do
              total += count
              count = in.read(buffer)

            total
        }

    // Example 2: UTF-8 decode (count decoded characters).
    suite(m"UTF-8 decode (4 MB)"):
      bench(m"Soundness  via(CharDecoder)")
        ( target = 1*Second, operationSize = textSize ):
        '{ turbulence.Benchmarks.textData.stream.via(summon[CharDecoder]).memoize.s.length }

      // The memoize row above concatenates the full 5 MB Text; this row counts
      // chars per window, the same aggregation shape as the FS2/ZIO rows.
      bench(m"Soundness  via(CharDecoder) fold")(target = 1*Second, operationSize = textSize):
        '{
            var total = 0L

            turbulence.Benchmarks.textData.stream.via(summon[CharDecoder])
            . drain(region => range => total += (range: Interval).size)

            total
        }

      bench(m"FS2  text.utf8.decode")(target = 1*Second, operationSize = textSize):
        '{
            import cats.effect.unsafe.implicits.global
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.textArray)).covary[cats.effect.IO]
            . through(fs2.text.utf8.decode).map(_.length).compile.fold(0)(_ + _).unsafeRunSync()
        }

      bench(m"ZIO  ZPipeline.utfDecode")(target = 1*Second, operationSize = textSize):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.textArray))
              . via(zio.stream.ZPipeline.utfDecode).map(_.length).runSum
        }

    // Line splitting: UTF-8 decode then split the 4 MB corpus into lines,
    // counting them. All three allocate one string per line — `delineate` emits
    // boxed `Array[Text]^{}` windows and counts records per window, but each
    // record is still a `Text`. (An earlier comment here claimed Soundness had
    // "no per-line intermediate"; it did, and the row was slower than FS2 for
    // it.) The interesting differences are how many times each line is copied on
    // the way into that string, and how the separator is found: FS2 slices
    // strings its decoder already built, with the intrinsified `String.indexOf`.
    suite(m"Line splitting (4 MB)"):
      bench(m"Soundness  Stream.delineate")
        ( target = 1*Second, operationSize = textSize ):
        '{
            var total = 0L
            turbulence.Benchmarks.textData.stream.delineate.drain(region => range => total += (range: Interval).size)
            total
        }

      bench(m"FS2  text.lines")(target = 1*Second, operationSize = textSize):
        '{
            import cats.effect.unsafe.implicits.global
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.textArray)).covary[cats.effect.IO]
            . through(fs2.text.utf8.decode).through(fs2.text.lines).compile.count.unsafeRunSync()
        }

      bench(m"ZIO  ZPipeline.splitLines")(target = 1*Second, operationSize = textSize):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.textArray))
              . via(zio.stream.ZPipeline.utfDecode).via(zio.stream.ZPipeline.splitLines).runCount
        }

    // Example 3: byte checksum fold. The Soundness fold runs over the raw window
    // with no per-element boxing; FS2/ZIO/Kyo box each byte as it flows.
    suite(m"Byte checksum fold (4 MB)"):
      bench(m"Soundness  drain")
        ( target = 1*Second, operationSize = size ):
        '{
            var total = 0L
            turbulence.Benchmarks.input.stream.drain: region =>
              range => region.visit(range) { index => total += (region(index) & 0xff) }
            total
        }

      bench(m"FS2  compile.fold")(target = 1*Second, operationSize = size):
        '{
            import cats.effect.unsafe.implicits.global
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.inputArray)).covary[cats.effect.IO]
            . compile.fold(0L)((acc, b) => acc + (b & 0xff)).unsafeRunSync()
        }

      bench(m"ZIO  runFold")(target = 1*Second, operationSize = size):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.inputArray))
              . runFold(0L)((acc, b) => acc + (b & 0xff))
        }

      bench(m"Kyo  Stream.fold")(target = 1*Second, operationSize = size):
        '{
            import kyo.*
            Stream.init(turbulence.Benchmarks.inputSeq).fold(0L)((acc, b) => acc + (b & 0xff)).eval
        }

    // Chained example A: gzip compress -> decompress roundtrip (identity on length).
    suite(m"Chained: gzip -> gunzip roundtrip (4 MB)"):
      bench(m"Soundness  compress[Gzip].decompress[Gzip]")
        ( target = 1*Second, operationSize = size ):
        '{ turbulence.Benchmarks.input.stream.compress[Gzip].decompress[Gzip].memoize.length }

      bench(m"FS2  gzip.gunzip")(target = 1*Second, operationSize = size):
        '{
            import cats.effect.unsafe.implicits.global
            val comp = fs2.compression.Compression.forSync[cats.effect.IO]
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.inputArray)).covary[cats.effect.IO]
            . through(comp.gzip()).through(comp.gunzip()).flatMap(_.content)
            . compile.count.unsafeRunSync()
        }

      bench(m"ZIO  gzip.gunzip")(target = 1*Second, operationSize = size):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.inputArray))
              . via(zio.stream.ZPipeline.gzip()).via(zio.stream.ZPipeline.gunzip())
              . runCount
        }

    // Chained example B: UTF-8 decode -> re-encode transcode roundtrip.
    suite(m"Chained: UTF-8 decode -> encode transcode (4 MB)"):
      bench(m"Soundness  via(dec).via(enc)")
        ( target = 1*Second, operationSize = textSize ):
        '{
            turbulence.Benchmarks.textData.stream
            . via(summon[CharDecoder]).via(summon[CharEncoder]).memoize.length
        }

      bench(m"FS2  utf8.decode.encode")(target = 1*Second, operationSize = textSize):
        '{
            import cats.effect.unsafe.implicits.global
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.textArray)).covary[cats.effect.IO]
            . through(fs2.text.utf8.decode).through(fs2.text.utf8.encode)
            . compile.count.unsafeRunSync()
        }

      bench(m"ZIO  utfDecode.utf8Encode")(target = 1*Second, operationSize = textSize):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.textArray))
              . via(zio.stream.ZPipeline.utfDecode).via(zio.stream.ZPipeline.utf8Encode)
              . runCount
        }

    // Chained example C: gunzip -> UTF-8 decode -> count characters.
    suite(m"Chained: gunzip -> UTF-8 decode -> count (gzipped text)"):
      bench(m"Soundness  decompress.via(summon[CharDecoder])")
        ( target = 1*Second, operationSize = textSize ):
        '{
            turbulence.Benchmarks.gzippedText.stream.decompress[Gzip]
            . via(summon[CharDecoder]).memoize.s.length
        }

      bench(m"FS2  gunzip.utf8.decode")(target = 1*Second, operationSize = textSize):
        '{
            import cats.effect.unsafe.implicits.global
            val comp = fs2.compression.Compression.forSync[cats.effect.IO]
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.gzippedTextArray)).covary[cats.effect.IO]
            . through(comp.gunzip()).flatMap(_.content)
            . through(fs2.text.utf8.decode).map(_.length).compile.fold(0)(_ + _).unsafeRunSync()
        }

      bench(m"ZIO  gunzip.utfDecode")(target = 1*Second, operationSize = textSize):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.gzippedTextArray))
              . via(zio.stream.ZPipeline.gunzip()).via(zio.stream.ZPipeline.utfDecode)
              . map(_.length).runSum
        }

    // Chained example O: gzip -> base64 -> debase64 -> gunzip. The "armored
    // transport" roundtrip: Soundness runs monotonous `Alphabet` ducts between
    // its compression ducts; FS2 its native base64 pipes; the JDK composes
    // `GZIPOutputStream` inside a `Base64` wrapping stream and mirrors it back.
    suite(m"Chained: gzip -> base64 -> decode -> gunzip (4 MB)"):
      bench(m"Soundness  compress.b64.b64.decompress")
        ( target = 1*Second, operationSize = size ):
        '{
            turbulence.Benchmarks.input.stream.compress[Gzip]
            . serialize[Base64]
            . deserialize[Base64]
            . decompress[Gzip].memoize.length
        }

      bench(m"FS2  gzip.base64.base64.gunzip")(target = 1*Second, operationSize = size):
        '{
            import cats.effect.unsafe.implicits.global
            val comp = fs2.compression.Compression.forSync[cats.effect.IO]
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.inputArray)).covary[cats.effect.IO]
            . through(comp.gzip())
            . through(fs2.text.base64.encode)
            . through(fs2.text.base64.decode)
            . through(comp.gunzip()).flatMap(_.content)
            . compile.count.unsafeRunSync()
        }

      bench(m"JDK  GZIP/Base64 stream composition")(target = 1*Second, operationSize = size):
        '{
            val buffer = new java.io.ByteArrayOutputStream(turbulence.Benchmarks.input.length/2)
            val out = new java.util.zip.GZIPOutputStream(java.util.Base64.getEncoder.wrap(buffer))
            out.write(turbulence.Benchmarks.inputArray)
            out.close()

            val in = new java.util.zip.GZIPInputStream
              (java.util.Base64.getDecoder.wrap(java.io.ByteArrayInputStream(buffer.toByteArray)))

            val scratch = new scala.Array[Byte](65536)
            var total = 0
            var count = in.read(scratch)

            while count >= 0 do
              total += count
              count = in.read(scratch)

            total
        }

    // Chained example P: gzip -> AES encrypt -> base64 -> decode -> decrypt ->
    // gunzip. The "secure archive" chain: Soundness streams the compression and
    // base64 legs and applies the cipher as whole-value enigmatic operations;
    // the JDK reference composes GZIP, Cipher and Base64 streams. FS2/ZIO have
    // no native cipher, so (as in the AES suite) only the JDK is shown.
    suite(m"Chained: gzip -> AES -> base64 -> decode -> decrypt -> gunzip (4 MB)"):
      bench(m"Soundness  full secure-archive chain")
        ( target = 1*Second, operationSize = size ):
        '{
            turbulence.Benchmarks.aesKey.uncloak:
              val compressed: Data = turbulence.Benchmarks.input.stream.compress[Gzip].memoize
              val encrypted: Data = compressed.encrypt(InitializationVector.random)

              val recovered: Data =
                encrypted.stream
                . serialize[Base64]
                . deserialize[Base64]
                . memoize

              val decrypted: Data = recovered.decrypt[Data, Aes[256] over Cbc against Pkcs7]
              decrypted.stream.decompress[Gzip].memoize.length
        }

      bench(m"JDK  GZIP/Cipher/Base64 composition")(target = 1*Second, operationSize = size):
        '{
            def cipher(mode: Int): javax.crypto.Cipher =
              val cipher = javax.crypto.Cipher.getInstance("AES/CBC/PKCS5Padding")

              cipher.init
                ( mode,
                  javax.crypto.spec.SecretKeySpec(turbulence.Benchmarks.jdkKeyBytes, "AES"),
                  javax.crypto.spec.IvParameterSpec(turbulence.Benchmarks.jdkIvBytes) )

              cipher

            val buffer = new java.io.ByteArrayOutputStream(turbulence.Benchmarks.input.length/2)

            val out = new java.util.zip.GZIPOutputStream
              (javax.crypto.CipherOutputStream
                (java.util.Base64.getEncoder.wrap(buffer), cipher(javax.crypto.Cipher.ENCRYPT_MODE)))

            out.write(turbulence.Benchmarks.inputArray)
            out.close()

            val in = new java.util.zip.GZIPInputStream
              (javax.crypto.CipherInputStream
                (java.util.Base64.getDecoder.wrap(java.io.ByteArrayInputStream(buffer.toByteArray)),
                 cipher(javax.crypto.Cipher.DECRYPT_MODE)))

            val scratch = new scala.Array[Byte](65536)
            var total = 0
            var count = in.read(scratch)

            while count >= 0 do
              total += count
              count = in.read(scratch)

            total
        }

    // Chained example Q: transcode cascade (no compression, 3-way). A long chain
    // of the one non-compression stage all three kernels share natively — UTF-8
    // transcoding — isolating the streaming machinery with no gzip to dominate
    // and no per-element boxing to skew it.
    suite(m"Chained: UTF-8 transcode cascade, no compression (4 MB)"):
      bench(m"Soundness  dec.enc.dec.enc.dec")
        ( target = 1*Second, operationSize = textSize ):
        '{
            turbulence.Benchmarks.textData.stream
            . via(summon[CharDecoder]).via(summon[CharEncoder])
            . via(summon[CharDecoder]).via(summon[CharEncoder])
            . via(summon[CharDecoder]).memoize.s.length
        }

      bench(m"FS2  utf8 decode/encode x2.5")(target = 1*Second, operationSize = textSize):
        '{
            import cats.effect.unsafe.implicits.global
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.textArray)).covary[cats.effect.IO]
            . through(fs2.text.utf8.decode).through(fs2.text.utf8.encode)
            . through(fs2.text.utf8.decode).through(fs2.text.utf8.encode)
            . through(fs2.text.utf8.decode)
            . map(_.length).compile.fold(0)(_ + _).unsafeRunSync()
        }

      bench(m"ZIO  utfDecode/utf8Encode x2.5")(target = 1*Second, operationSize = textSize):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.textArray))
              . via(zio.stream.ZPipeline.utfDecode).via(zio.stream.ZPipeline.utf8Encode)
              . via(zio.stream.ZPipeline.utfDecode).via(zio.stream.ZPipeline.utf8Encode)
              . via(zio.stream.ZPipeline.utfDecode)
              . map(_.length).runSum
        }

    // Chained example R: transcode + base64 armor (no compression, 2-way).
    // FS2 has a native streaming base64 pipe; ZIO-Streams has none, so only FS2
    // is the streaming reference here.
    suite(m"Chained: transcode + base64 armor, no compression (4 MB)"):
      bench(m"Soundness  dec.enc.b64.b64.dec")
        ( target = 1*Second, operationSize = textSize ):
        '{
            turbulence.Benchmarks.textData.stream
            . via(summon[CharDecoder]).via(summon[CharEncoder])
            . serialize[Base64].deserialize[Base64]
            . via(summon[CharDecoder]).memoize.s.length
        }

      bench(m"FS2  utf8/base64 chain")(target = 1*Second, operationSize = textSize):
        '{
            import cats.effect.unsafe.implicits.global
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.textArray)).covary[cats.effect.IO]
            . through(fs2.text.utf8.decode).through(fs2.text.utf8.encode)
            . through(fs2.text.base64.encode).through(fs2.text.base64.decode)
            . through(fs2.text.utf8.decode)
            . map(_.length).compile.fold(0)(_ + _).unsafeRunSync()
        }

    // Chained example S: slice + transcode (no compression, 3-way), combining
    // the `discard`/`truncate`/`gather` kernel operators with transcoding. The
    // slice bounds are chunk-aware byte counts in all three libraries; the
    // terminal count accumulates over whole windows.
    suite(m"Chained: drop -> transcode -> take -> count (4 MB)"):
      bench(m"Soundness  discard.dec.enc.truncate.gather")
        ( target = 1*Second, operationSize = textSize ):
        '{
            turbulence.Benchmarks.textData.stream.discard(turbulence.Benchmarks.dropBytes)
            . via(summon[CharDecoder]).via(summon[CharEncoder])
            . truncate(turbulence.Benchmarks.takeBytes)
            . gather(0L)(_ => (total, range) => total + (range: Interval).size)
        }

      bench(m"FS2  drop.utf8.take.count")(target = 1*Second, operationSize = textSize):
        '{
            import cats.effect.unsafe.implicits.global
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.textArray)).covary[cats.effect.IO]
            . drop(turbulence.Benchmarks.dropBytes)
            . through(fs2.text.utf8.decode).through(fs2.text.utf8.encode)
            . take(turbulence.Benchmarks.takeBytes)
            . compile.count.unsafeRunSync()
        }

      bench(m"ZIO  drop.utf.take.runCount")(target = 1*Second, operationSize = textSize):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.textArray))
              . drop(turbulence.Benchmarks.dropBytes)
              . via(zio.stream.ZPipeline.utfDecode).via(zio.stream.ZPipeline.utf8Encode)
              . take(turbulence.Benchmarks.takeBytes)
              . runCount
        }

    // Example D: base64 encode. Soundness `monotonous` base-N is a whole-value
    // operation; FS2 has a native streaming base64 pipe; the JDK is the
    // universal reference. ZIO/Kyo have no native base64.
    suite(m"Base64 encode (4 MB)"):
      bench(m"Soundness  serialize[Base64]")
        ( target = 1*Second, operationSize = size ):
        '{ turbulence.Benchmarks.input.serialize[Base64].s.length }

      bench(m"FS2  text.base64.encode")(target = 1*Second, operationSize = size):
        '{
            import cats.effect.unsafe.implicits.global
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.inputArray)).covary[cats.effect.IO]
            . through(fs2.text.base64.encode).map(_.length).compile.fold(0)(_ + _).unsafeRunSync()
        }

      bench(m"JDK  java.util.Base64")(target = 1*Second, operationSize = size):
        '{ java.util.Base64.getEncoder.encodeToString(turbulence.Benchmarks.inputArray).length }

    // Isolated streaming base64 encode+decode roundtrip, measuring the duct
    // directly (Data -> Text -> Data) against FS2's native base64 pipes.
    suite(m"Streaming base64 encode+decode roundtrip (4 MB)"):
      bench(m"Soundness  serialize.deserialize")
        ( target = 1*Second, operationSize = size ):
        '{
            turbulence.Benchmarks.input.stream
            . serialize[Base64].deserialize[Base64].memoize.length
        }

      bench(m"FS2  base64.encode.decode")(target = 1*Second, operationSize = size):
        '{
            import cats.effect.unsafe.implicits.global
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.inputArray)).covary[cats.effect.IO]
            . through(fs2.text.base64.encode).through(fs2.text.base64.decode)
            . compile.count.unsafeRunSync()
        }

    // Example E: AES-256-CBC encrypt. Soundness `enigmatic` streaming encryption
    // drives the JCE cipher over the legacy `Chain` view. ZIO/FS2/Kyo have no
    // native block cipher, so only the JDK reference is shown.
    suite(m"AES-256-CBC encrypt (4 MB)"):
      bench(m"Soundness  enigmatic encryptStream")
        ( target = 1*Second, operationSize = size ):
        '{
            turbulence.Benchmarks.aesKey.uncloak:
              Chain(turbulence.Benchmarks.input).encrypt(InitializationVector.random)
              . fold(0L)(_ + _.length)
        }

      bench(m"JDK  javax.crypto.Cipher")(target = 1*Second, operationSize = size):
        '{
            val cipher = javax.crypto.Cipher.getInstance("AES/CBC/PKCS5Padding")
            cipher.init
              ( javax.crypto.Cipher.ENCRYPT_MODE,
                javax.crypto.spec.SecretKeySpec(turbulence.Benchmarks.jdkKeyBytes, "AES"),
                javax.crypto.spec.IvParameterSpec(turbulence.Benchmarks.jdkIvBytes) )
            cipher.doFinal(turbulence.Benchmarks.inputArray).length
        }

    // Example F: public `read` typeclass path vs the bare kernel `memoize`.
    suite(m"Public read vs kernel memoize (4 MB)"):
      bench(m"Soundness  Stream.memoize (kernel)")
        ( target = 1*Second, operationSize = size ):
        '{ turbulence.Benchmarks.inputChunks.stdlib.iterator.stream.memoize.length }

      bench(m"Soundness  read[Data]")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.inputChunks.read[Data].length }

      bench(m"Soundness  read[Text] (decode)")(target = 1*Second, operationSize = textSize):
        '{ turbulence.Benchmarks.textData.read[Text].s.length }

    // Example G: `Buffering` block-size sensitivity for the gzip staging buffer.
    suite(m"Buffering block-size sweep: gzip (4 MB)"):
      bench(m"block 4 KiB (standard)")
        ( target = 1*Second, operationSize = size ):
        '{
            turbulence.Benchmarks.input.stream
            . compress[Gzip](using summon, turbulence.Benchmarks.buffering(4096)).memoize.length
        }

      bench(m"block 512 B")(target = 1*Second, operationSize = size):
        '{
            turbulence.Benchmarks.input.stream
            . compress[Gzip](using summon, turbulence.Benchmarks.buffering(512)).memoize.length
        }

      bench(m"block 64 KiB")(target = 1*Second, operationSize = size):
        '{
            turbulence.Benchmarks.input.stream
            . compress[Gzip](using summon, turbulence.Benchmarks.buffering(65536)).memoize.length
        }

    // Example H: `Cursor` parser pull-loop (peek/next) vs a bare `Array[Byte]` scan.
    suite(m"Cursor pull-loop vs raw array scan (4 MB)"):
      bench(m"Soundness  Cursor peek/next")
        ( target = 1*Second, operationSize = size ):
        '{
            val cursor = Cursor[Data](turbulence.Benchmarks.input)
            var total = 0L
            while !cursor.finished do
              total += cursor.peek.asInt
              cursor.next()
            total
        }

      bench(m"Raw  scala.Array[Byte] loop")(target = 1*Second, operationSize = size):
        '{
            val array = turbulence.Benchmarks.inputArray
            var total = 0L
            var i = 0
            while i < array.length do { total += (array(i) & 0xff); i += 1 }
            total
        }

    // Example I: `writeTo` sink path vs a raw OutputStream write.
    suite(m"writeTo sink vs raw OutputStream write (4 MB)"):
      bench(m"Soundness  writeTo")
        ( target = 1*Second, operationSize = size ):
        '{
            val out = new java.io.ByteArrayOutputStream(turbulence.Benchmarks.input.length)
            turbulence.Benchmarks.inputChunks.writeTo(out)
            out.size
        }

      bench(m"Raw  OutputStream.write")(target = 1*Second, operationSize = size):
        '{
            val out = new java.io.ByteArrayOutputStream(turbulence.Benchmarks.input.length)
            turbulence.Benchmarks.inputChunks.each(chunk => out.write(chunk.asInstanceOf[scala.Array[Byte]]))
            out.size
        }

    // Example J: hex / base32 encode (cost by base), with the JDK `HexFormat` as
    // a hex reference. Base32 has no common JDK/FS2 counterpart.
    suite(m"Hex / Base32 encode (4 MB)"):
      bench(m"Soundness  serialize[Hex]")
        ( target = 1*Second, operationSize = size ):
        '{ turbulence.Benchmarks.input.serialize[Hex].s.length }

      bench(m"JDK  HexFormat")(target = 1*Second, operationSize = size):
        '{ java.util.HexFormat.of.formatHex(turbulence.Benchmarks.inputArray).length }

      bench(m"Soundness  serialize[Base32]")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.input.serialize[Base32].s.length }

    // Example K: `pump` pump (pull -> push OutputStream sink) vs `memoize`.
    suite(m"pump pump vs memoize (4 MB)"):
      bench(m"Soundness  Stream.memoize")
        ( target = 1*Second, operationSize = size ):
        '{ turbulence.Benchmarks.input.stream.memoize.length }

      bench(m"Soundness  pump(sink)")(target = 1*Second, operationSize = size):
        '{
            val out = new java.io.ByteArrayOutputStream(turbulence.Benchmarks.input.length)
            turbulence.Benchmarks.input.stream
            . pump(summon[java.io.OutputStream is Sink by Data over Credit].intake(out))
            out.size
        }

    // Example L: `Conduit` cross-thread hand-off — a bounded SPSC boundary,
    // producing 4 MB in 64 KiB chunks on one thread and consuming on another.
    // The reference queues pass chunk references with zero copy; `Conduit`
    // shares a `Data` chunk's immutable backing by reference likewise.
    suite(m"Conduit cross-thread hand-off (4 MB in 64 KiB chunks)"):
      bench(m"Soundness  Conduit")
        ( target = 1*Second, operationSize = size ):
        '{
            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.each(intake.put)
              intake.finish())
            var total = 0L
            stream.drain(region => range => total += (range: Interval).size)
            producer.join()
            total
        }

      // The same hand-off with the consumer on a virtual thread too, so both
      // endpoints suspend fiber-style on the carrier pool. The row above runs its
      // consumer on the harness's own platform worker, making it the mixed pair
      // `Conduit`'s header warns about — one side's kernel parks dominate.
      bench(m"Soundness  Conduit VT both")(target = 1*Second, operationSize = size):
        '{
            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.each(intake.put)
              intake.finish())
            var total = 0L
            val consumer = Thread.ofVirtual.start(() =>
              stream.drain(region => range => total += (range: Interval).size))
            consumer.join()
            producer.join()
            total
        }

      // Both endpoints virtual, and a ring deep enough to hold the whole 64-chunk
      // corpus so the producer never parks at all. `Buffering`'s own note records
      // that hand-off throughput keeps improving well past the default depth of
      // 16, and that a queue covering a whole burst more than doubled it — this
      // row is what that advice is worth here, and it is the closest structural
      // match to the Kyo row's `putBatch`/`takeExactly` pair.
      bench(m"Soundness  Conduit VT depth 64")(target = 1*Second, operationSize = size):
        '{
            given deepBuffering: Buffering:
              def capacity(substrate: Substrate): Int = substrate match
                case Substrate.Bytes  => 4096
                case Substrate.Chars  => 2048
                case Substrate.Boxes  => 256

              def depth: Int = 64

            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.each(intake.put)
              intake.finish())
            var total = 0L
            val consumer = Thread.ofVirtual.start(() =>
              stream.drain(region => range => total += (range: Interval).size))
            consumer.join()
            producer.join()
            total
        }

      bench(m"JDK  ArrayBlockingQueue")(target = 1*Second, operationSize = size):
        '{
            val queue = new java.util.concurrent.ArrayBlockingQueue[AnyRef](8)
            val end = new Object
            val producer = new Thread(() =>
              turbulence.Benchmarks.inputChunks.each(chunk => queue.put(chunk.asInstanceOf[AnyRef]))
              queue.put(end))
            producer.start()
            var total = 0L
            var running = true
            while running do
              val item = queue.take()
              if item eq end then running = false else total += item.asInstanceOf[Data].length
            producer.join()
            total
        }

      bench(m"FS2  Channel.bounded")(target = 1*Second, operationSize = size):
        '{
            import cats.effect.unsafe.implicits.global
            import cats.effect.IO, cats.syntax.all.*
            val program = fs2.concurrent.Channel.bounded[IO, fs2.Chunk[Byte]](8).flatMap: channel =>
              val produce =
                turbulence.Benchmarks.inputChunkList.foldLeft(IO.unit): (io, chunk) =>
                  io *> channel.send(fs2.Chunk.array(chunk.asInstanceOf[scala.Array[Byte]])).void
                *> channel.close.void
              produce.start *> channel.stream.compile.fold(0L)((acc, chunk) => acc + chunk.size)
            program.unsafeRunSync()
        }

      bench(m"ZIO  Queue.bounded")(target = 1*Second, operationSize = size):
        '{
            turbulence.Benchmarks.runZio:
              import zio.*, zio.stream.*
              val source =
                ZStream.fromIterable
                  (turbulence.Benchmarks.inputChunkList.map(c => Chunk.fromArray(c.asInstanceOf[scala.Array[Byte]])))
              for
                queue <- Queue.bounded[Take[Nothing, Chunk[Byte]]](8)
                _     <- source.runIntoQueue(queue).fork
                total <- ZStream.fromQueue(queue).flattenTake.runFold(0L)((acc, c) => acc + c.size)
              yield total
        }

      bench(m"Kyo  Channel")(target = 1*Second, operationSize = size):
        '{
            import kyo.*
            import AllowUnsafe.embrace.danger

            val program =
              for
                channel  <- Channel.initUnscoped[AnyRef](8)
                producer <- Fiber.initUnscoped:
                              channel.putBatch(turbulence.Benchmarks.inputChunkList.asInstanceOf[scala.collection.immutable.List[AnyRef]])
                chunks   <- channel.takeExactly(turbulence.Benchmarks.inputChunkList.length)
                _        <- producer.get
              yield chunks.foldLeft(0L): (acc, chunk) =>
                acc + chunk.asInstanceOf[Data].length

            Abort.run(KyoApp.Unsafe.runAndBlock(Duration.Infinity)(program)).eval.getOrThrow
        }

    // Example M: `Confluence` fan-in — merge four streams. A stable in-memory
    // source's window is shared by reference, exactly as the references pass
    // immutable chunks.
    suite(m"Confluence fan-in: merge 4 streams (4 MB)"):
      bench(m"Soundness  Confluence")
        ( target = 1*Second, operationSize = size ):
        '{
            supervise:
              val merged = Confluence(turbulence.Benchmarks.quarters.map(q => q.stream)*)
              var total = 0L
              merged.drain(region => range => total += (range: Interval).size)
              total
        }

      bench(m"FS2  parJoinUnbounded")(target = 1*Second, operationSize = size):
        '{
            import cats.effect.unsafe.implicits.global
            import cats.effect.IO
            val streams =
              turbulence.Benchmarks.quarters.map: q =>
                fs2.Stream.chunk(fs2.Chunk.array(q.asInstanceOf[scala.Array[Byte]])).covary[IO]
            fs2.Stream.emits(streams).parJoinUnbounded.compile.count.unsafeRunSync()
        }

      bench(m"ZIO  mergeAllUnbounded")(target = 1*Second, operationSize = size):
        '{
            turbulence.Benchmarks.runZio:
              import zio.*, zio.stream.*
              val streams =
                turbulence.Benchmarks.quarters.map(q => ZStream.fromChunk(Chunk.fromArray(q.asInstanceOf[scala.Array[Byte]])))
              ZStream.mergeAllUnbounded()(streams*).runCount
        }

      bench(m"Kyo  Stream.collectAll")(target = 1*Second, operationSize = size):
        '{
            import kyo.*
            import AllowUnsafe.embrace.danger

            val streams =
              turbulence.Benchmarks.quarters.map: quarter =>
                Stream.init:
                  scala.collection.immutable.ArraySeq.unsafeWrapArray
                    (quarter.asInstanceOf[scala.Array[Byte]])

            val program =
              Stream.collectAll(streams.toSeq)
              . mapChunkPure { chunk => scala.collection.immutable.Seq(chunk.size.toLong) }
              . fold(0L)(_ + _)

            Abort.run(KyoApp.Unsafe.runAndBlock(Duration.Infinity)(program)).eval.getOrThrow
        }

    // Example N: `Divergence` fan-out — broadcast to three consumers. The source
    // chunk is shared read-only between subscribers, so it too passes without
    // copying; the residual gap on fan-out is the thread-vs-fiber wakeup of the
    // subscriber consumers.
    suite(m"Divergence fan-out: broadcast to 3 (4 MB in, 12 MB consumed)"):
      bench(m"Soundness  Divergence")
        ( target = 1*Second, operationSize = size ):
        '{
            supervise:
              val subscribers = Divergence(turbulence.Benchmarks.input.stream, 3)
              val tasks = subscribers.map: subscriber =>
                async:
                  var total = 0L
                  subscriber.drain(region => range => total += (range: Interval).size)
                  total
              tasks.map(_.await()).sum
        }

      bench(m"FS2  broadcastThrough")(target = 1*Second, operationSize = size):
        '{
            import cats.effect.unsafe.implicits.global
            import cats.effect.IO, cats.syntax.all.*
            val counter: fs2.Pipe[IO, Byte, Long] = _.chunks.foldMap(chunk => chunk.size.toLong)
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.inputArray)).covary[IO]
            . broadcastThrough(counter, counter, counter).compile.foldMonoid.unsafeRunSync()
        }

      bench(m"ZIO  broadcast")(target = 1*Second, operationSize = size):
        '{
            turbulence.Benchmarks.runZio:
              import zio.*, zio.stream.*
              ZIO.scoped:
                ZStream.fromChunk(Chunk.fromArray(turbulence.Benchmarks.inputArray)).broadcast(3, 16).flatMap: streams =>
                  ZIO.foreachPar(streams)(_.runCount).map(_.sum)
        }

    // Example N2: ring-depth sweep for the cross-thread hand-off — the tuning
    // data behind `Buffering.depth`'s conservative default of 16 (its comment
    // records hand-off throughput still improving at 64). Depth multiplies the
    // worst-case in-flight blocks of every copy-path conduit, so the win must
    // justify the memory; each row is Example L's pipeline with only the ring
    // depth varied.
      bench(m"Kyo  broadcast3")(target = 1*Second, operationSize = size):
        '{
            import kyo.*
            import AllowUnsafe.embrace.danger

            def count(stream: Stream[Byte, Async]): Long < Async =
              stream.mapChunkPure { chunk => scala.collection.immutable.Seq(chunk.size.toLong) }
              . fold(0L)(_ + _)

            val program =
              Scope.run:
                Stream.init(turbulence.Benchmarks.inputSeq).broadcast3(16).map: (s1, s2, s3) =>
                  Async.zip(count(s1), count(s2), count(s3)).map { (a, b, c) => a + b + c }

            Abort.run(KyoApp.Unsafe.runAndBlock(Duration.Infinity)(program)).eval.getOrThrow
        }

    suite(m"Conduit depth sweep (4 MB in 64 KiB chunks)"):
      bench(m"depth 2")(target = 1*Second, operationSize = size):
        '{
            given Buffering = turbulence.Benchmarks.buffering(4096, 2)
            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.each(intake.put)
              intake.finish())
            var total = 0L
            stream.drain(region => range => total += (range: Interval).size)
            producer.join()
            total
        }

      bench(m"depth 4")(target = 1*Second, operationSize = size):
        '{
            given Buffering = turbulence.Benchmarks.buffering(4096, 4)
            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.each(intake.put)
              intake.finish())
            var total = 0L
            stream.drain(region => range => total += (range: Interval).size)
            producer.join()
            total
        }

      bench(m"depth 16 (standard)")(target = 1*Second, operationSize = size):
        '{
            given Buffering = turbulence.Benchmarks.buffering(4096, 16)
            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.each(intake.put)
              intake.finish())
            var total = 0L
            stream.drain(region => range => total += (range: Interval).size)
            producer.join()
            total
        }

      bench(m"depth 64")(target = 1*Second, operationSize = size):
        '{
            given Buffering = turbulence.Benchmarks.buffering(4096, 64)
            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.each(intake.put)
              intake.finish())
            var total = 0L
            stream.drain(region => range => total += (range: Interval).size)
            producer.join()
            total
        }

      bench(m"depth 256")(target = 1*Second, operationSize = size):
        '{
            given Buffering = turbulence.Benchmarks.buffering(4096, 256)
            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.each(intake.put)
              intake.finish())
            var total = 0L
            stream.drain(region => range => total += (range: Interval).size)
            producer.join()
            total
        }

    // Example N3: the price and payoff of backpressure. Every row funnels the
    // same 4 MB through a cross-thread hand-off to a consumer slowed by a fixed
    // amount of per-block CPU work (`burn` — data-independent, so every rival's
    // consumer drags identically). Throughput is consumer-gated for every row;
    // the differentiating columns are allocation and peak heap, where the
    // unbounded models buffer the producer's entire lead and the bounded ones
    // hold it to the ring.
    suite(m"Backpressure vs unbounded model: slow consumer (4 MB)"):
      bench(m"Soundness  Conduit depth 16")(target = 1*Second, operationSize = size):
        '{
            val (intake, stream) = Conduit[Data]()

            // The producer signals end-of-stream even if it dies: an
            // `OutOfMemoryError` in this thread is invisible to the harness's
            // OOM tolerance, which catches only in the worker's own thread, and
            // a consumer parked on a hand-off whose producer died silently
            // waits forever — wedging the whole sweep at its `join`. `finish`
            // allocates nothing, so the `finally` cannot itself fail.
            val producer = Thread.ofVirtual.start(() =>
              try
                turbulence.Benchmarks.inputChunks.each: chunk =>
                  intake.put(turbulence.Benchmarks.freshChunk(chunk))
              catch case _: java.lang.OutOfMemoryError => ()
              finally intake.finish())

            var total = 0L
            stream.drain: region =>
              range =>
                val count = (range: Interval).size
                total += count + (turbulence.Benchmarks.burn(count) & 1L)
            producer.join()
            total
        }

      bench(m"Unbounded  LinkedBlockingQueue")(target = 1*Second, operationSize = size):
        '{
            val queue = new java.util.concurrent.LinkedBlockingQueue[AnyRef]()
            val end = new Object

            // This row's whole point is to blow up the heap, and the blowup
            // lands in THIS thread (it does the allocating), where the
            // harness's OOM tolerance cannot see it. The sentinel must still
            // reach the consumer or it blocks in `take` forever and the sweep
            // wedges at `join` — so it goes in a `finally`, and because
            // `put` itself allocates a queue node, it retries until the
            // consumer has drained enough for one node to fit.
            val producer = Thread.ofVirtual.start(() =>
              try
                turbulence.Benchmarks.inputChunks.each: chunk =>
                  queue.put(turbulence.Benchmarks.freshChunk(chunk).asInstanceOf[AnyRef])
              catch case _: java.lang.OutOfMemoryError => ()
              finally
                var sent = false
                while !sent do
                  try
                    queue.put(end)
                    sent = true
                  catch case _: java.lang.OutOfMemoryError => Thread.sleep(10))
            var total = 0L
            var running = true
            while running do
              val item = queue.take()
              if item eq end then running = false else
                val count = item.asInstanceOf[Data].length
                total += count + (turbulence.Benchmarks.burn(count) & 1L)
            producer.join()
            total
        }

      bench(m"Unbounded  Relay[Data]")(target = 1*Second, operationSize = size):
        '{
            val relay = Relay[Data]()

            // As the queue row above: the termination must arrive even if the
            // producer dies of the OOM it is built to cause, and `stop` puts a
            // sentinel — an allocation — so it retries.
            val producer = Thread.ofVirtual.start(() =>
              try
                turbulence.Benchmarks.inputChunks.each: chunk =>
                  relay.put(turbulence.Benchmarks.freshChunk(chunk))
              catch case _: java.lang.OutOfMemoryError => ()
              finally
                var sent = false
                while !sent do
                  try
                    relay.stop()
                    sent = true
                  catch case _: java.lang.OutOfMemoryError => Thread.sleep(10))
            var total = 0L
            relay.stream.records.each: chunk =>
              total += chunk.length + (turbulence.Benchmarks.burn(chunk.length) & 1L)
            producer.join()
            total
        }

      bench(m"FS2  Channel.unbounded")(target = 1*Second, operationSize = size):
        '{
            import cats.effect.unsafe.implicits.global
            import cats.effect.IO, cats.syntax.all.*
            val program = fs2.concurrent.Channel.unbounded[IO, fs2.Chunk[Byte]].flatMap: channel =>
              val produce =
                turbulence.Benchmarks.inputChunkList.foldLeft(IO.unit): (io, chunk) =>
                  io *> channel.send(fs2.Chunk.array(
                    turbulence.Benchmarks.freshChunk(chunk).asInstanceOf[scala.Array[Byte]])).void
                *> channel.close.void
              produce.start *> channel.stream.compile.fold(0L): (acc, chunk) =>
                acc + chunk.size + (turbulence.Benchmarks.burn(chunk.size) & 1L)
            program.unsafeRunSync()
        }

      bench(m"ZIO  Queue.unbounded")(target = 1*Second, operationSize = size):
        '{
            turbulence.Benchmarks.runZio:
              import zio.*, zio.stream.*
              val source =
                ZStream.fromIterable
                  (turbulence.Benchmarks.inputChunkList.map: c =>
                    Chunk.fromArray(turbulence.Benchmarks.freshChunk(c).asInstanceOf[scala.Array[Byte]]))
              for
                queue <- Queue.unbounded[Take[Nothing, Chunk[Byte]]]
                // `ensuring` runs on defects too, so a producer-side
                // `OutOfMemoryError` (a defect) still delivers end-of-stream
                // rather than stranding the consumer.
                _     <- source.runIntoQueue(queue).ensuring(queue.offer(Take.end)).fork
                total <- ZStream.fromQueue(queue).flattenTake.runFold(0L): (acc, c) =>
                           acc + c.size + (turbulence.Benchmarks.burn(c.size) & 1L)
              yield total
        }

    // Example N4: fan-out with one dragging subscriber. Every library gates the
    // source on its slowest subscriber (the correct replication semantics), so
    // these rows quantify what that gating costs each of them when one of three
    // consumers carries the Example N3 per-block drag.
    suite(m"Divergence with one slow subscriber (4 MB in, 12 MB consumed)"):
      bench(m"Soundness  Divergence")(target = 1*Second, operationSize = size):
        '{
            supervise:
              val ticket = new java.util.concurrent.atomic.AtomicInteger(0)
              val subscribers = Divergence(turbulence.Benchmarks.input.stream, 3)
              val tasks = subscribers.map: subscriber =>
                async:
                  val slow = ticket.getAndIncrement() == 0
                  var total = 0L
                  subscriber.drain: region =>
                    range =>
                      val count = (range: Interval).size
                      total += count
                      if slow then total += turbulence.Benchmarks.burn(count) & 1L
                  total
              tasks.map(_.await()).sum
        }

      bench(m"FS2  broadcastThrough")(target = 1*Second, operationSize = size):
        '{
            import cats.effect.unsafe.implicits.global
            import cats.effect.IO, cats.syntax.all.*
            val counter: fs2.Pipe[IO, Byte, Long] = _.chunks.foldMap(chunk => chunk.size.toLong)
            val slow: fs2.Pipe[IO, Byte, Long] = _.chunks.foldMap: chunk =>
              chunk.size.toLong + (turbulence.Benchmarks.burn(chunk.size) & 1L)
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.inputArray)).covary[IO]
            . broadcastThrough(slow, counter, counter).compile.foldMonoid.unsafeRunSync()
        }

      bench(m"ZIO  broadcast")(target = 1*Second, operationSize = size):
        '{
            turbulence.Benchmarks.runZio:
              import zio.*, zio.stream.*
              ZIO.scoped:
                ZStream.fromChunk(Chunk.fromArray(turbulence.Benchmarks.inputArray)).broadcast(3, 16).flatMap: streams =>
                  ZIO.foreachPar(streams.zipWithIndex): (stream, index) =>
                    if index == 0 then
                      stream.chunks.runFold(0L): (acc, chunk) =>
                        acc + chunk.size + (turbulence.Benchmarks.burn(chunk.size) & 1L)
                    else stream.runCount
                  . map(_.sum)
        }

    // Example O: stress rows — the memory profile of the cross-thread hand-off
    // pipeline from Example L, run as 16 concurrent pipelines for a fixed
    // wall-clock window. The throughput rows above measure speed; these measure
    // the axis the bounded-buffer design targets: allocation per pipeline, peak
    // heap, and a retained live set that stays flat under concurrency.
    suite(m"Stress: cross-thread hand-off memory (4 MB in 64 KiB chunks, N=16)"):
      import threading.platformThreading

      stress(m"Soundness  Conduit")(target = 2*Second, concurrency = 16):
        '{
            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.each(intake.put)
              intake.finish())
            var total = 0L
            stream.drain(region => range => total += (range: Interval).size)
            producer.join()
            total
        }

      // The same pipeline with both endpoints on virtual threads: every hand-off
      // park is a fiber-style suspension on the carrier pool rather than a kernel
      // park, so sixteen pipelines no longer oversubscribe the OS scheduler. The
      // row above and the fiber runtimes below are within a few percent of each
      // other at this concurrency; this one runs about 2.7x faster than any of
      // them, which is the whole finding — the suite was missing a row for the
      // configuration `Conduit` documents as the one to use under load.
      stress(m"Soundness  Conduit VT both")(target = 2*Second, concurrency = 16):
        '{
            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.each(intake.put)
              intake.finish())
            var total = 0L
            val consumer = Thread.ofVirtual.start(() =>
              stream.drain(region => range => total += (range: Interval).size))
            consumer.join()
            producer.join()
            total
        }

      stress(m"FS2  Channel.bounded")(target = 2*Second, concurrency = 16):
        '{
            import cats.effect.unsafe.implicits.global
            import cats.effect.IO
            val program = fs2.concurrent.Channel.bounded[IO, fs2.Chunk[Byte]](8).flatMap: channel =>
              val produce =
                turbulence.Benchmarks.inputChunkList.foldLeft(IO.unit): (io, chunk) =>
                  io *> channel.send(fs2.Chunk.array(chunk.asInstanceOf[scala.Array[Byte]])).void
                *> channel.close.void
              produce.start *> channel.stream.compile.fold(0L)((acc, chunk) => acc + chunk.size)
            program.unsafeRunSync()
        }

      stress(m"ZIO  Queue.bounded")(target = 2*Second, concurrency = 16):
        '{
            turbulence.Benchmarks.runZio:
              import zio.*, zio.stream.*
              val source =
                ZStream.fromIterable
                  (turbulence.Benchmarks.inputChunkList.map(c => Chunk.fromArray(c.asInstanceOf[scala.Array[Byte]])))
              for
                queue <- Queue.bounded[Take[Nothing, Chunk[Byte]]](8)
                _     <- source.runIntoQueue(queue).fork
                total <- ZStream.fromQueue(queue).flattenTake.runFold(0L)((acc, c) => acc + c.size)
              yield total
        }
      stress(m"Kyo  Channel")(target = 2*Second, concurrency = 16):
        '{
            import kyo.*
            import AllowUnsafe.embrace.danger

            val program =
              for
                channel  <- Channel.initUnscoped[AnyRef](8)
                producer <- Fiber.initUnscoped:
                              channel.putBatch(turbulence.Benchmarks.inputChunkList.asInstanceOf[scala.collection.immutable.List[AnyRef]])
                chunks   <- channel.takeExactly(turbulence.Benchmarks.inputChunkList.length)
                _        <- producer.get
              yield chunks.foldLeft(0L): (acc, chunk) =>
                acc + chunk.asInstanceOf[Data].length

            Abort.run(KyoApp.Unsafe.runAndBlock(Duration.Infinity)(program)).eval.getOrThrow
        }

    // Example P: constrained-heap scaling sweep — the same hand-off pipeline in a
    // pinned 128 MB heap, with the pipeline count doubling from 1 towards 64.
    // Each step is reported as its own row, so the table reads as the
    // throughput/latency/memory-vs-N curve, and the sweep stops at the largest N
    // the heap sustains (OutOfMemoryError, or over half the window spent in GC) —
    // the bounded-buffer design should sustain more pipelines in the same heap.
    suite(m"Stress: constrained-heap scaling sweep (128 MB heap, N ≤ 64)"):
      import threading.platformThreading

      constrained(m"Soundness  Conduit")(target = 1*Second, sweep = 64):
        '{
            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.each(intake.put)
              intake.finish())
            var total = 0L
            stream.drain(region => range => total += (range: Interval).size)
            producer.join()
            total
        }

      // The same pipeline with BOTH endpoints on virtual threads, so every
      // hand-off park is a fiber-style suspension on the carrier pool rather
      // than a kernel park of a platform worker — the scheduling model the
      // fiber runtimes (Kyo, ZIO) scale on. 2N virtual threads multiplex on
      // ~cores carriers, so pipeline count no longer oversubscribes the OS
      // scheduler.
      constrained(m"Soundness  Conduit VT both")(target = 1*Second, sweep = 64):
        '{
            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.each(intake.put)
              intake.finish())
            var total = 0L
            val consumer = Thread.ofVirtual.start(() =>
              stream.drain(region => range => total += (range: Interval).size))
            consumer.join()
            producer.join()
            total
        }

      constrained(m"FS2  Channel.bounded")(target = 1*Second, sweep = 64):
        '{
            import cats.effect.unsafe.implicits.global
            import cats.effect.IO
            val program = fs2.concurrent.Channel.bounded[IO, fs2.Chunk[Byte]](8).flatMap: channel =>
              val produce =
                turbulence.Benchmarks.inputChunkList.foldLeft(IO.unit): (io, chunk) =>
                  io *> channel.send(fs2.Chunk.array(chunk.asInstanceOf[scala.Array[Byte]])).void
                *> channel.close.void
              produce.start *> channel.stream.compile.fold(0L)((acc, chunk) => acc + chunk.size)
            program.unsafeRunSync()
        }

      constrained(m"ZIO  Queue.bounded")(target = 1*Second, sweep = 64):
        '{
            turbulence.Benchmarks.runZio:
              import zio.*, zio.stream.*
              val source =
                ZStream.fromIterable
                  (turbulence.Benchmarks.inputChunkList.map(c => Chunk.fromArray(c.asInstanceOf[scala.Array[Byte]])))
              for
                queue <- Queue.bounded[Take[Nothing, Chunk[Byte]]](8)
                _     <- source.runIntoQueue(queue).fork
                total <- ZStream.fromQueue(queue).flattenTake.runFold(0L)((acc, c) => acc + c.size)
              yield total
        }

    // Example P2: what the absence of backpressure costs. The Example N3
    // pipeline — a producer racing ahead of a CPU-slowed consumer — in a pinned
    // 128 MB heap, doubling the pipeline count towards 64. A bounded hand-off
    // holds each pipeline's in-flight data to its ring, so the sweep should keep
    // climbing; the unbounded models buffer each producer's entire 4 MB lead, so
    // their sweeps are expected to die early on OutOfMemoryError or GC thrash —
    // the largest N each row reaches is the finding.
      constrained(m"Kyo  Channel")(target = 1*Second, sweep = 64):
        '{
            import kyo.*
            import AllowUnsafe.embrace.danger

            val program =
              for
                channel  <- Channel.initUnscoped[AnyRef](8)
                producer <- Fiber.initUnscoped:
                              channel.putBatch(turbulence.Benchmarks.inputChunkList.asInstanceOf[scala.collection.immutable.List[AnyRef]])
                chunks   <- channel.takeExactly(turbulence.Benchmarks.inputChunkList.length)
                _        <- producer.get
              yield chunks.foldLeft(0L): (acc, chunk) =>
                acc + chunk.asInstanceOf[Data].length

            Abort.run(KyoApp.Unsafe.runAndBlock(Duration.Infinity)(program)).eval.getOrThrow
        }

    suite(m"Stress: unbounded-model blowup (slow consumer, 128 MB heap, N ≤ 64)"):
      import threading.platformThreading

      constrained(m"Soundness  Conduit depth 16")(target = 1*Second, sweep = 64):
        '{
            val (intake, stream) = Conduit[Data]()

            // The producer signals end-of-stream even if it dies: an
            // `OutOfMemoryError` in this thread is invisible to the harness's
            // OOM tolerance, which catches only in the worker's own thread, and
            // a consumer parked on a hand-off whose producer died silently
            // waits forever — wedging the whole sweep at its `join`. `finish`
            // allocates nothing, so the `finally` cannot itself fail.
            val producer = Thread.ofVirtual.start(() =>
              try
                turbulence.Benchmarks.inputChunks.each: chunk =>
                  intake.put(turbulence.Benchmarks.freshChunk(chunk))
              catch case _: java.lang.OutOfMemoryError => ()
              finally intake.finish())

            var total = 0L
            stream.drain: region =>
              range =>
                val count = (range: Interval).size
                total += count + (turbulence.Benchmarks.burn(count) & 1L)
            producer.join()
            total
        }

      constrained(m"Unbounded  LinkedBlockingQueue")(target = 1*Second, sweep = 64):
        '{
            val queue = new java.util.concurrent.LinkedBlockingQueue[AnyRef]()
            val end = new Object

            // This row's whole point is to blow up the heap, and the blowup
            // lands in THIS thread (it does the allocating), where the
            // harness's OOM tolerance cannot see it. The sentinel must still
            // reach the consumer or it blocks in `take` forever and the sweep
            // wedges at `join` — so it goes in a `finally`, and because
            // `put` itself allocates a queue node, it retries until the
            // consumer has drained enough for one node to fit.
            val producer = Thread.ofVirtual.start(() =>
              try
                turbulence.Benchmarks.inputChunks.each: chunk =>
                  queue.put(turbulence.Benchmarks.freshChunk(chunk).asInstanceOf[AnyRef])
              catch case _: java.lang.OutOfMemoryError => ()
              finally
                var sent = false
                while !sent do
                  try
                    queue.put(end)
                    sent = true
                  catch case _: java.lang.OutOfMemoryError => Thread.sleep(10))
            var total = 0L
            var running = true
            while running do
              val item = queue.take()
              if item eq end then running = false else
                val count = item.asInstanceOf[Data].length
                total += count + (turbulence.Benchmarks.burn(count) & 1L)
            producer.join()
            total
        }

      constrained(m"Unbounded  Relay[Data]")(target = 1*Second, sweep = 64):
        '{
            val relay = Relay[Data]()

            // As the queue row above: the termination must arrive even if the
            // producer dies of the OOM it is built to cause, and `stop` puts a
            // sentinel — an allocation — so it retries.
            val producer = Thread.ofVirtual.start(() =>
              try
                turbulence.Benchmarks.inputChunks.each: chunk =>
                  relay.put(turbulence.Benchmarks.freshChunk(chunk))
              catch case _: java.lang.OutOfMemoryError => ()
              finally
                var sent = false
                while !sent do
                  try
                    relay.stop()
                    sent = true
                  catch case _: java.lang.OutOfMemoryError => Thread.sleep(10))
            var total = 0L
            relay.stream.records.each: chunk =>
              total += chunk.length + (turbulence.Benchmarks.burn(chunk.length) & 1L)
            producer.join()
            total
        }

      // Known exposure, unlike its siblings: cats-effect does not run
      // finalizers on fatal throwables, so no `guarantee` can deliver
      // `channel.close` after a producer-fiber `OutOfMemoryError`, and a
      // consumer could strand as the thread-based rows once did. In practice
      // the allocation happens inside `send`'s effect on the calling fiber and
      // surfaces as the sweep's expected OOM; if this row ever wedges a run,
      // this is why.
      constrained(m"FS2  Channel.unbounded")(target = 1*Second, sweep = 64):
        '{
            import cats.effect.unsafe.implicits.global
            import cats.effect.IO, cats.syntax.all.*
            val program = fs2.concurrent.Channel.unbounded[IO, fs2.Chunk[Byte]].flatMap: channel =>
              val produce =
                turbulence.Benchmarks.inputChunkList.foldLeft(IO.unit): (io, chunk) =>
                  io *> channel.send(fs2.Chunk.array(
                    turbulence.Benchmarks.freshChunk(chunk).asInstanceOf[scala.Array[Byte]])).void
                *> channel.close.void
              produce.start *> channel.stream.compile.fold(0L): (acc, chunk) =>
                acc + chunk.size + (turbulence.Benchmarks.burn(chunk.size) & 1L)
            program.unsafeRunSync()
        }

      constrained(m"ZIO  Queue.unbounded")(target = 1*Second, sweep = 64):
        '{
            turbulence.Benchmarks.runZio:
              import zio.*, zio.stream.*
              val source =
                ZStream.fromIterable
                  (turbulence.Benchmarks.inputChunkList.map: c =>
                    Chunk.fromArray(turbulence.Benchmarks.freshChunk(c).asInstanceOf[scala.Array[Byte]]))
              for
                queue <- Queue.unbounded[Take[Nothing, Chunk[Byte]]]
                // `ensuring` runs on defects too, so a producer-side
                // `OutOfMemoryError` (a defect) still delivers end-of-stream
                // rather than stranding the consumer.
                _     <- source.runIntoQueue(queue).ensuring(queue.offer(Take.end)).fork
                total <- ZStream.fromQueue(queue).flattenTake.runFold(0L): (acc, c) =>
                           acc + c.size + (turbulence.Benchmarks.burn(c.size) & 1L)
              yield total
        }

    // Example P3: the same slow-consumer pipeline at a fixed N=16 in the roomy
    // default heap — the headline retained/peakHeap comparison. The bounded
    // conduit's live set stays flat at the ring bound; the unbounded models'
    // grows with the producer's lead, and the retained column shows it.
    suite(m"Stress: retained memory under slow consumption (4 MB, N=16)"):
      import threading.platformThreading

      stress(m"Soundness  Conduit depth 16")(target = 2*Second, concurrency = 16):
        '{
            val (intake, stream) = Conduit[Data]()

            // The producer signals end-of-stream even if it dies: an
            // `OutOfMemoryError` in this thread is invisible to the harness's
            // OOM tolerance, which catches only in the worker's own thread, and
            // a consumer parked on a hand-off whose producer died silently
            // waits forever — wedging the whole sweep at its `join`. `finish`
            // allocates nothing, so the `finally` cannot itself fail.
            val producer = Thread.ofVirtual.start(() =>
              try
                turbulence.Benchmarks.inputChunks.each: chunk =>
                  intake.put(turbulence.Benchmarks.freshChunk(chunk))
              catch case _: java.lang.OutOfMemoryError => ()
              finally intake.finish())

            var total = 0L
            stream.drain: region =>
              range =>
                val count = (range: Interval).size
                total += count + (turbulence.Benchmarks.burn(count) & 1L)
            producer.join()
            total
        }

      stress(m"Unbounded  LinkedBlockingQueue")(target = 2*Second, concurrency = 16):
        '{
            val queue = new java.util.concurrent.LinkedBlockingQueue[AnyRef]()
            val end = new Object

            // This row's whole point is to blow up the heap, and the blowup
            // lands in THIS thread (it does the allocating), where the
            // harness's OOM tolerance cannot see it. The sentinel must still
            // reach the consumer or it blocks in `take` forever and the sweep
            // wedges at `join` — so it goes in a `finally`, and because
            // `put` itself allocates a queue node, it retries until the
            // consumer has drained enough for one node to fit.
            val producer = Thread.ofVirtual.start(() =>
              try
                turbulence.Benchmarks.inputChunks.each: chunk =>
                  queue.put(turbulence.Benchmarks.freshChunk(chunk).asInstanceOf[AnyRef])
              catch case _: java.lang.OutOfMemoryError => ()
              finally
                var sent = false
                while !sent do
                  try
                    queue.put(end)
                    sent = true
                  catch case _: java.lang.OutOfMemoryError => Thread.sleep(10))
            var total = 0L
            var running = true
            while running do
              val item = queue.take()
              if item eq end then running = false else
                val count = item.asInstanceOf[Data].length
                total += count + (turbulence.Benchmarks.burn(count) & 1L)
            producer.join()
            total
        }

      stress(m"Unbounded  Relay[Data]")(target = 2*Second, concurrency = 16):
        '{
            val relay = Relay[Data]()

            // As the queue row above: the termination must arrive even if the
            // producer dies of the OOM it is built to cause, and `stop` puts a
            // sentinel — an allocation — so it retries.
            val producer = Thread.ofVirtual.start(() =>
              try
                turbulence.Benchmarks.inputChunks.each: chunk =>
                  relay.put(turbulence.Benchmarks.freshChunk(chunk))
              catch case _: java.lang.OutOfMemoryError => ()
              finally
                var sent = false
                while !sent do
                  try
                    relay.stop()
                    sent = true
                  catch case _: java.lang.OutOfMemoryError => Thread.sleep(10))
            var total = 0L
            relay.stream.records.each: chunk =>
              total += chunk.length + (turbulence.Benchmarks.burn(chunk.length) & 1L)
            producer.join()
            total
        }

    // Example Q: gzip-decompression memory — the pipeline from Example 1b run as
    // 8 concurrent pipelines. The compressed corpus is small, so the measured
    // allocation is almost entirely output-side handling of the inflated 4 MB —
    // precisely what recycled conduit blocks eliminate, against per-chunk output
    // allocation. Every row drains without retaining: the Soundness body counts
    // output bytes through `sweep`, the same shape as FS2's `compile.count` and
    // ZIO's `runCount`. (Compression is the wrong direction to measure here: this
    // corpus deflates to a few dozen kB, so the output-side allocation vanishes
    // and the figure is dominated by how each library ingests the 4 MB input.)
    suite(m"Stress: gzip decompression memory (4 MB out, N=8)"):
      import threading.platformThreading

      stress(m"Soundness  Stream.decompress[Gzip]")
        ( target = 2*Second, concurrency = 8 ):
        '{
            var total = 0L

            turbulence.Benchmarks.gzippedInput.stream.decompress[Gzip]
            . drain(region => range => total += (range: Interval).size)

            total
        }

      stress(m"FS2  Compression[IO].gunzip")(target = 2*Second, concurrency = 8):
        '{
            import cats.effect.unsafe.implicits.global
            val comp = fs2.compression.Compression.forSync[cats.effect.IO]
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.gzippedInputArray))
            . covary[cats.effect.IO]
            . through(comp.gunzip()).flatMap(_.content)
            . compile.count.unsafeRunSync()
        }

      stress(m"ZIO  ZPipeline.gunzip")(target = 2*Second, concurrency = 8):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.gzippedInputArray))
              . via(zio.stream.ZPipeline.gunzip())
              . runCount
        }

    // Example R: UTF-8 decode memory — the pipeline from Example 2 run as 8
    // concurrent pipelines. All three rows aggregate decoded character counts per
    // chunk (the fold shape), so the contrast is the per-chunk text allocation of
    // the decode stage itself.
    suite(m"Stress: UTF-8 decode memory (4 MB, N=8)"):
      import threading.platformThreading

      stress(m"Soundness  via(CharDecoder)")
        ( target = 2*Second, concurrency = 8 ):
        '{
            var total = 0L

            turbulence.Benchmarks.textData.stream.via(summon[CharDecoder])
            . drain(region => range => total += (range: Interval).size)

            total
        }

      stress(m"FS2  text.utf8.decode")(target = 2*Second, concurrency = 8):
        '{
            import cats.effect.unsafe.implicits.global
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.textArray)).covary[cats.effect.IO]
            . through(fs2.text.utf8.decode).map(_.length).compile.fold(0)(_ + _).unsafeRunSync()
        }

      stress(m"ZIO  ZPipeline.utfDecode")(target = 2*Second, concurrency = 8):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.textArray))
              . via(zio.stream.ZPipeline.utfDecode).map(_.length).runSum
        }

    // Example S: SLO-constrained capacity search — the maximum sustained hand-off
    // throughput such that 99% of operations complete within 5 ms, in a 2 GB heap
    // on 4 CPUs (advisory on macOS; see `BenchmarkDevice.invoke`). The search
    // doubles the pipeline count while each window meets the target,
    // binary-searches the compliant/non-compliant boundary to ~12% resolution,
    // then confirms the winner over a window three times longer. The probes form
    // the curve; the `(sustained, N = …)` row is the answer: each library's
    // maximum sustained ops/sec under identical constraints.
    suite(m"Stress: capacity search (99% ≤ 5 ms, 2 GB heap, 4 CPUs)"):
      locally:
        import threading.platformThreading

        gated(m"Soundness  Conduit")
          ( target = 1*Second, threshold = 5*Milli(Second), compliance = 99 ):
          '{
              val (intake, stream) = Conduit[Data]()
              val producer = Thread.ofVirtual.start(() =>
                turbulence.Benchmarks.inputChunks.each(intake.put)
                intake.finish())
              var total = 0L
              stream.drain(region => range => total += (range: Interval).size)
              producer.join()
              total
          }

        gated(m"FS2  Channel.bounded")
          ( target = 1*Second, threshold = 5*Milli(Second), compliance = 99 ):
          '{
              import cats.effect.unsafe.implicits.global
              import cats.effect.IO
              val program = fs2.concurrent.Channel.bounded[IO, fs2.Chunk[Byte]](8).flatMap: channel =>
                val produce =
                  turbulence.Benchmarks.inputChunkList.foldLeft(IO.unit): (io, chunk) =>
                    io *> channel.send(fs2.Chunk.array(chunk.asInstanceOf[scala.Array[Byte]])).void
                  *> channel.close.void
                produce.start *> channel.stream.compile.fold(0L)((acc, chunk) => acc + chunk.size)
              program.unsafeRunSync()
          }

        gated(m"ZIO  Queue.bounded")
          ( target = 1*Second, threshold = 5*Milli(Second), compliance = 99 ):
          '{
              turbulence.Benchmarks.runZio:
                import zio.*, zio.stream.*
                val source =
                  ZStream.fromIterable
                    (turbulence.Benchmarks.inputChunkList.map(c => Chunk.fromArray(c.asInstanceOf[scala.Array[Byte]])))
                for
                  queue <- Queue.bounded[Take[Nothing, Chunk[Byte]]](8)
                  _     <- source.runIntoQueue(queue).fork
                  total <- ZStream.fromQueue(queue).flattenTake.runFold(0L)((acc, c) => acc + c.size)
                yield total
          }

        gated(m"Kyo  Channel")
          ( target = 1*Second, threshold = 5*Milli(Second), compliance = 99 ):
          '{
              import kyo.*
              import AllowUnsafe.embrace.danger

              val program =
                for
                  channel  <- Channel.initUnscoped[AnyRef](8)
                  producer <- Fiber.initUnscoped:
                                channel.putBatch(turbulence.Benchmarks.inputChunkList.asInstanceOf[scala.collection.immutable.List[AnyRef]])
                  chunks   <- channel.takeExactly(turbulence.Benchmarks.inputChunkList.length)
                  _        <- producer.get
                yield chunks.foldLeft(0L): (acc, chunk) =>
                  acc + chunk.asInstanceOf[Data].length

              Abort.run(KyoApp.Unsafe.runAndBlock(Duration.Infinity)(program)).eval.getOrThrow
          }

      // The same Soundness pipeline with the harness workers on virtual threads
      // (the file's ambient `virtualThreading`, where the rows above pin
      // `platformThreading`): pipelines multiplex over the carrier pool instead
      // of one OS thread each, the model a massively-concurrent application
      // would use — and the fair comparison against the fiber runtimes'
      // sustained concurrency.
      gated(m"Soundness  Conduit (virtual workers)")
        ( target = 1*Second, threshold = 5*Milli(Second), compliance = 99 ):
        '{
            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.each(intake.put)
              intake.finish())
            var total = 0L
            stream.drain(region => range => total += (range: Interval).size)
            producer.join()
            total
        }

    // Examples U–X: saturated whole-pipeline comparisons — the three-way rows above
    // measure single pipelines, and Example S searches for capacity, but only for the
    // hand-off primitive. These promote four representative pipelines to saturated
    // three-way rows, on all cores (no `cpus` gate: the machine itself is the resource
    // under test), in 256 KiB operations (see the small corpora). Each pipeline gets
    // two suites: a sweep, doubling the pipeline count from 1 to 128 so the table reads
    // as each library's throughput-vs-N curve up to and past core count; and a capacity
    // search for the maximum sustained rate with 99% of operations within 10 ms — each
    // library's headline ops/sec figure on a saturated machine. One uniform SLO keeps
    // the pipelines comparable; 10 ms is roughly ten times a 256 KiB operation's serial
    // latency. The four cover distinct regimes: gzip decompression (CPU-bound compute),
    // line splitting (allocation-heavy text), the transcode cascade (pure streaming
    // machinery, five stages) and fan-in (the one pipeline that is itself concurrent,
    // so scheduler runs on scheduler). Not promoted: base64 (no ZIO codec, so 2-way
    // only), Brotli (no rivals), the checksum fold (a per-element boxing microbenchmark,
    // not a pipeline) and Divergence fan-out (the same class as fan-in).

    // Example U: saturated gzip decompression.
    suite(m"Stress: saturated gzip decompression sweep (256 KiB, N ≤ 128)"):
      import threading.platformThreading

      saturated(m"Soundness  Stream.decompress[Gzip]")(target = 1*Second, sweep = 128):
        '{
            var total = 0L

            turbulence.Benchmarks.smallGzipped.stream.decompress[Gzip]
            . drain(region => range => total += (range: Interval).size)

            total
        }

      saturated(m"FS2  Compression[IO].gunzip")(target = 1*Second, sweep = 128):
        '{
            import cats.effect.unsafe.implicits.global
            val comp = fs2.compression.Compression.forSync[cats.effect.IO]
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.smallGzippedArray))
            . covary[cats.effect.IO]
            . through(comp.gunzip()).flatMap(_.content)
            . compile.count.unsafeRunSync()
        }

      saturated(m"ZIO  ZPipeline.gunzip")(target = 1*Second, sweep = 128):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.smallGzippedArray))
              . via(zio.stream.ZPipeline.gunzip())
              . runCount
        }

    suite(m"Stress: saturated gzip decompression capacity (99% ≤ 10 ms, 256 KiB)"):
      locally:
        import threading.platformThreading

        saturated(m"Soundness  Stream.decompress[Gzip]")
          ( target = 1*Second, threshold = 10*Milli(Second), compliance = 99 ):
          '{
              var total = 0L

              turbulence.Benchmarks.smallGzipped.stream.decompress[Gzip]
              . drain(region => range => total += (range: Interval).size)

              total
          }

        saturated(m"FS2  Compression[IO].gunzip")
          ( target = 1*Second, threshold = 10*Milli(Second), compliance = 99 ):
          '{
              import cats.effect.unsafe.implicits.global
              val comp = fs2.compression.Compression.forSync[cats.effect.IO]
              fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.smallGzippedArray))
              . covary[cats.effect.IO]
              . through(comp.gunzip()).flatMap(_.content)
              . compile.count.unsafeRunSync()
          }

        saturated(m"ZIO  ZPipeline.gunzip")
          ( target = 1*Second, threshold = 10*Milli(Second), compliance = 99 ):
          '{
              turbulence.Benchmarks.runZio:
                zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.smallGzippedArray))
                . via(zio.stream.ZPipeline.gunzip())
                . runCount
          }

      // The harness workers on virtual threads (the file's ambient
      // `virtualThreading`), as in Example S: pipelines multiplex over the carrier
      // pool instead of one OS thread each — the fair comparison against the fiber
      // runtimes' sustained concurrency.
      saturated(m"Soundness  Stream.decompress[Gzip] (virtual workers)")
        ( target = 1*Second, threshold = 10*Milli(Second), compliance = 99 ):
        '{
            var total = 0L

            turbulence.Benchmarks.smallGzipped.stream.decompress[Gzip]
            . drain(region => range => total += (range: Interval).size)

            total
        }

    // Example V: saturated line splitting (UTF-8 decode + split).
    suite(m"Stress: saturated line splitting sweep (256 KiB, N ≤ 128)"):
      import threading.platformThreading

      saturated(m"Soundness  Stream.delineate")(target = 1*Second, sweep = 128):
        '{
            var total = 0L
            turbulence.Benchmarks.smallText.stream.delineate.drain(region => range => total += (range: Interval).size)
            total
        }

      saturated(m"FS2  text.lines")(target = 1*Second, sweep = 128):
        '{
            import cats.effect.unsafe.implicits.global
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.smallTextArray)).covary[cats.effect.IO]
            . through(fs2.text.utf8.decode).through(fs2.text.lines).compile.count.unsafeRunSync()
        }

      saturated(m"ZIO  ZPipeline.splitLines")(target = 1*Second, sweep = 128):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.smallTextArray))
              . via(zio.stream.ZPipeline.utfDecode).via(zio.stream.ZPipeline.splitLines).runCount
        }

    suite(m"Stress: saturated line splitting capacity (99% ≤ 10 ms, 256 KiB)"):
      import threading.platformThreading

      saturated(m"Soundness  Stream.delineate")
        ( target = 1*Second, threshold = 10*Milli(Second), compliance = 99 ):
        '{
            var total = 0L
            turbulence.Benchmarks.smallText.stream.delineate.drain(region => range => total += (range: Interval).size)
            total
        }

      saturated(m"FS2  text.lines")
        ( target = 1*Second, threshold = 10*Milli(Second), compliance = 99 ):
        '{
            import cats.effect.unsafe.implicits.global
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.smallTextArray)).covary[cats.effect.IO]
            . through(fs2.text.utf8.decode).through(fs2.text.lines).compile.count.unsafeRunSync()
        }

      saturated(m"ZIO  ZPipeline.splitLines")
        ( target = 1*Second, threshold = 10*Milli(Second), compliance = 99 ):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.smallTextArray))
              . via(zio.stream.ZPipeline.utfDecode).via(zio.stream.ZPipeline.splitLines).runCount
        }

    // Example W: saturated UTF-8 transcode cascade. Unlike chained example Q's
    // `memoize` row, every library aggregates counts per window (the fold shape), so
    // no row retains its output.
    suite(m"Stress: saturated transcode cascade sweep (256 KiB, N ≤ 128)"):
      import threading.platformThreading

      saturated(m"Soundness  dec.enc.dec.enc.dec")(target = 1*Second, sweep = 128):
        '{
            var total = 0L

            turbulence.Benchmarks.smallText.stream
            . via(summon[CharDecoder]).via(summon[CharEncoder])
            . via(summon[CharDecoder]).via(summon[CharEncoder])
            . via(summon[CharDecoder])
            . drain(region => range => total += (range: Interval).size)

            total
        }

      saturated(m"FS2  utf8 decode/encode x2.5")(target = 1*Second, sweep = 128):
        '{
            import cats.effect.unsafe.implicits.global
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.smallTextArray)).covary[cats.effect.IO]
            . through(fs2.text.utf8.decode).through(fs2.text.utf8.encode)
            . through(fs2.text.utf8.decode).through(fs2.text.utf8.encode)
            . through(fs2.text.utf8.decode)
            . map(_.length).compile.fold(0)(_ + _).unsafeRunSync()
        }

      saturated(m"ZIO  utfDecode/utf8Encode x2.5")(target = 1*Second, sweep = 128):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.smallTextArray))
              . via(zio.stream.ZPipeline.utfDecode).via(zio.stream.ZPipeline.utf8Encode)
              . via(zio.stream.ZPipeline.utfDecode).via(zio.stream.ZPipeline.utf8Encode)
              . via(zio.stream.ZPipeline.utfDecode)
              . map(_.length).runSum
        }

    suite(m"Stress: saturated transcode cascade capacity (99% ≤ 10 ms, 256 KiB)"):
      import threading.platformThreading

      saturated(m"Soundness  dec.enc.dec.enc.dec")
        ( target = 1*Second, threshold = 10*Milli(Second), compliance = 99 ):
        '{
            var total = 0L

            turbulence.Benchmarks.smallText.stream
            . via(summon[CharDecoder]).via(summon[CharEncoder])
            . via(summon[CharDecoder]).via(summon[CharEncoder])
            . via(summon[CharDecoder])
            . drain(region => range => total += (range: Interval).size)

            total
        }

      saturated(m"FS2  utf8 decode/encode x2.5")
        ( target = 1*Second, threshold = 10*Milli(Second), compliance = 99 ):
        '{
            import cats.effect.unsafe.implicits.global
            fs2.Stream.chunk(fs2.Chunk.array(turbulence.Benchmarks.smallTextArray)).covary[cats.effect.IO]
            . through(fs2.text.utf8.decode).through(fs2.text.utf8.encode)
            . through(fs2.text.utf8.decode).through(fs2.text.utf8.encode)
            . through(fs2.text.utf8.decode)
            . map(_.length).compile.fold(0)(_ + _).unsafeRunSync()
        }

      saturated(m"ZIO  utfDecode/utf8Encode x2.5")
        ( target = 1*Second, threshold = 10*Milli(Second), compliance = 99 ):
        '{
            turbulence.Benchmarks.runZio:
              zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(turbulence.Benchmarks.smallTextArray))
              . via(zio.stream.ZPipeline.utfDecode).via(zio.stream.ZPipeline.utf8Encode)
              . via(zio.stream.ZPipeline.utfDecode).via(zio.stream.ZPipeline.utf8Encode)
              . via(zio.stream.ZPipeline.utfDecode)
              . map(_.length).runSum
        }

    // Example X: saturated fan-in. Each operation is itself concurrent (four sources
    // merging), so at high N this measures each library's scheduler multiplexing many
    // small concurrent merges — and almost all of the operation's cost is the *setup*
    // of that concurrency, since the stable sources share their windows by reference.
    // The suite-level `platformThreading` pins only the harness workers; the Soundness
    // bodies re-import `virtualThreading` so that `Confluence`'s internal pumps (one
    // strand per source, forked per operation) are virtual threads — the counterpart
    // of the rivals' per-merge fiber spawns, and the configuration a
    // massively-concurrent application would use. On platform threads the row measures
    // OS thread creation, ~two orders of magnitude dearer than a fiber spawn, not
    // merging. If this pipeline's serial latency proves to exceed a couple of
    // milliseconds, its SLO (alone) should rise to 20 ms.
    suite(m"Stress: saturated fan-in sweep (256 KiB over 4 streams, N ≤ 128)"):
      import threading.platformThreading

      saturated(m"Soundness  Confluence")(target = 1*Second, sweep = 128):
        '{
            import threading.virtualThreading
            supervise:
              val merged = Confluence(turbulence.Benchmarks.smallQuarters.map(q => q.stream)*)
              var total = 0L
              merged.drain(region => range => total += (range: Interval).size)
              total
        }

      saturated(m"FS2  parJoinUnbounded")(target = 1*Second, sweep = 128):
        '{
            import cats.effect.unsafe.implicits.global
            import cats.effect.IO
            val streams =
              turbulence.Benchmarks.smallQuarters.map: q =>
                fs2.Stream.chunk(fs2.Chunk.array(q.asInstanceOf[scala.Array[Byte]])).covary[IO]
            fs2.Stream.emits(streams).parJoinUnbounded.compile.count.unsafeRunSync()
        }

      saturated(m"ZIO  mergeAllUnbounded")(target = 1*Second, sweep = 128):
        '{
            turbulence.Benchmarks.runZio:
              import zio.*, zio.stream.*
              val streams =
                turbulence.Benchmarks.smallQuarters.map(q => ZStream.fromChunk(Chunk.fromArray(q.asInstanceOf[scala.Array[Byte]])))
              ZStream.mergeAllUnbounded()(streams*).runCount
        }

      saturated(m"Kyo  Stream.collectAll")(target = 1*Second, sweep = 128):
        '{
            import kyo.*
            import AllowUnsafe.embrace.danger

            val streams =
              turbulence.Benchmarks.smallQuarters.map: quarter =>
                Stream.init:
                  scala.collection.immutable.ArraySeq.unsafeWrapArray
                    (quarter.asInstanceOf[scala.Array[Byte]])

            val program =
              Stream.collectAll(streams.toSeq)
              . mapChunkPure { chunk => scala.collection.immutable.Seq(chunk.size.toLong) }
              . fold(0L)(_ + _)

            Abort.run(KyoApp.Unsafe.runAndBlock(Duration.Infinity)(program)).eval.getOrThrow
        }

    suite(m"Stress: saturated fan-in capacity (99% ≤ 10 ms, 256 KiB over 4 streams)"):
      import threading.platformThreading

      saturated(m"Soundness  Confluence")
        ( target = 1*Second, threshold = 10*Milli(Second), compliance = 99 ):
        '{
            import threading.virtualThreading
            supervise:
              val merged = Confluence(turbulence.Benchmarks.smallQuarters.map(q => q.stream)*)
              var total = 0L
              merged.drain(region => range => total += (range: Interval).size)
              total
        }

      saturated(m"FS2  parJoinUnbounded")
        ( target = 1*Second, threshold = 10*Milli(Second), compliance = 99 ):
        '{
            import cats.effect.unsafe.implicits.global
            import cats.effect.IO
            val streams =
              turbulence.Benchmarks.smallQuarters.map: q =>
                fs2.Stream.chunk(fs2.Chunk.array(q.asInstanceOf[scala.Array[Byte]])).covary[IO]
            fs2.Stream.emits(streams).parJoinUnbounded.compile.count.unsafeRunSync()
        }

      saturated(m"ZIO  mergeAllUnbounded")
        ( target = 1*Second, threshold = 10*Milli(Second), compliance = 99 ):
        '{
            turbulence.Benchmarks.runZio:
              import zio.*, zio.stream.*
              val streams =
                turbulence.Benchmarks.smallQuarters.map(q => ZStream.fromChunk(Chunk.fromArray(q.asInstanceOf[scala.Array[Byte]])))
              ZStream.mergeAllUnbounded()(streams*).runCount
        }

    // Separator scan: the byte duct's inner loop in isolation, over the 4 MB
    // corpus, one call per line. The question is whether replacing two
    // comparisons (and, with short-circuiting, two branches) per byte with a
    // mask-and-compare (one branch) pays, and whether biasing by two to narrow
    // the false-positive range from 8-15 to 10-13 pays for its extra operation.
    // Note this corpus has no tabs, which the five-bit mask admits and the
    // six-bit one does not — on tab-heavy text the biased variant would fare
    // relatively better than it does here.
      saturated(m"Kyo  Stream.collectAll")
        ( target = 1*Second, threshold = 10*Milli(Second), compliance = 99 ):
        '{
            import kyo.*
            import AllowUnsafe.embrace.danger

            val streams =
              turbulence.Benchmarks.smallQuarters.map: quarter =>
                Stream.init:
                  scala.collection.immutable.ArraySeq.unsafeWrapArray
                    (quarter.asInstanceOf[scala.Array[Byte]])

            val program =
              Stream.collectAll(streams.toSeq)
              . mapChunkPure { chunk => scala.collection.immutable.Seq(chunk.size.toLong) }
              . fold(0L)(_ + _)

            Abort.run(KyoApp.Unsafe.runAndBlock(Duration.Infinity)(program)).eval.getOrThrow
        }

    suite(m"Separator scan variants (4 MB)"):
      bench(m"Two comparisons per byte")(target = 1*Second, operationSize = textSize):
        '{ turbulence.Benchmarks.scanPairwise(turbulence.Benchmarks.textArray) }

      bench(m"Mask and compare (admits 8-15)")(target = 1*Second, operationSize = textSize):
        '{ turbulence.Benchmarks.scanMasked(turbulence.Benchmarks.textArray) }

      bench(m"Bias by two, mask and compare (admits 10-13)")
        ( target = 1*Second, operationSize = textSize ):
        '{ turbulence.Benchmarks.scanBiased(turbulence.Benchmarks.textArray) }

    // Example T: profiles — where the time actually goes in the pipelines the
    // stress suites measure. Each renders as a histogram of the hottest methods
    // (self time, from JFR execution samples), coloured by package.
    suite(m"Profile: pipeline hotspots"):
      // Line splitting is two stages — the UTF-8 decode duct and the separator
      // duct — so this profile attributes between them, and shows what remains
      // per line once the fast path has removed the second copy.
      profile(m"Stream.delineate (4 MB of text)")(target = 5*Second):
        '{
            var total = 0L

            turbulence.Benchmarks.textData.stream.delineate
            . drain(region => range => total += (range: Interval).size)

            total
        }

      profile(m"Conduit hand-off (4 MB in 64 KiB chunks)")(target = 5*Second):
        '{
            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.each(intake.put)
              intake.finish())
            var total = 0L
            stream.drain(region => range => total += (range: Interval).size)
            producer.join()
            total
        }

      profile(m"Stream.decompress[Gzip] (4 MB out)")(target = 5*Second):
        '{
            var total = 0L

            turbulence.Benchmarks.gzippedInput.stream.decompress[Gzip]
            . drain(region => range => total += (range: Interval).size)

            total
        }
