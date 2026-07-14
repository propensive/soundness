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
package turbulence

import ambience.*, environments.javaEnvironment, systems.javaSystem
import enigmatic.*, blockCipherMode.cbc, blockCipherPadding.pkcs7
import gastronomy.providers.javaStdlibProvider, gastronomy.crypto.permitUnauthenticatedCrypto
import parasite.*, threading.virtualThreading, probates.panicProbate
import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import hieroglyph.*, charDecoders.utf8Decoder, charEncoders.utf8Encoder,
    textSanitizers.strictSanitizer
import monotonous.*, alphabets.base64Standard, alphabets.hexLowerCase, alphabets.base32LowerCase
import prepositional.*
import probably.*
import proscenium.*
import quantitative.*
import sedentary.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import vacuous.*
import zephyrine.*

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
//   * Kyo has no gzip pipeline and no incremental UTF-8 decoder, so it appears
//     only in the checksum fold (like locomotion's "… only" rows).

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
  lazy val inputArray: Array[Byte] = input.asInstanceOf[Array[Byte]]
  lazy val inputSeq: scala.collection.immutable.ArraySeq[Byte] =
    scala.collection.immutable.ArraySeq.unsafeWrapArray(inputArray)
  // The same 4 MB split into 64 KiB chunks, so aggregation/write loops iterate
  // (a single in-memory chunk would let `read[Data]` fold to an identity).
  lazy val inputChunks: LazyList[Data] =
    LazyList.from((0 until input.length by 65536).map: offset =>
      input.slice(offset, (offset + 65536).min(input.length)))
  // The 4 MB split into four equal parts, one per source stream for fan-in.
  lazy val quarters: IndexedSeq[Data] =
    val q = input.length/4
    IndexedSeq.tabulate(4)(i => input.slice(i*q, if i == 3 then input.length else (i + 1)*q))

  // ~4 MB of UTF-8 text with multi-byte characters, so the decode exercises the
  // cross-chunk continuation path in every library.
  lazy val textData: Data =
    val unit = t"The quick brown fox — jümps over the lazy dog. café ☕ 数据 🚀\n"
    val builder = new java.lang.StringBuilder(4 << 20)
    while builder.length < (4 << 20) do builder.append(unit.s)
    Data(builder.toString.getBytes("UTF-8").nn*)
  lazy val textArray: Array[Byte] = textData.asInstanceOf[Array[Byte]]

  // The text corpus pre-compressed with gzip, for the "read a gzipped text
  // stream" chained pipeline.
  lazy val gzippedText: Data = textData.stream.compress[Gzip].memoize
  lazy val gzippedTextArray: Array[Byte] = gzippedText.asInstanceOf[Array[Byte]]

  // The byte corpus pre-compressed with gzip, for the standalone decompression
  // suite.
  lazy val gzippedInput: Data = input.stream.compress[Gzip].memoize
  lazy val gzippedInputArray: Array[Byte] = gzippedInput.asInstanceOf[Array[Byte]]

  // AES-256 key + a fixed key/IV for the JDK reference, generated/derived once.
  lazy val aesKey: SymmetricKey[Aes[256] over Cbc against Pkcs7] =
    SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
  lazy val jdkKeyBytes: Array[Byte] = Array.tabulate(32)(i => (i*7 + 1).toByte)
  lazy val jdkIvBytes:  Array[Byte] = Array.tabulate(16)(i => (i*13 + 3).toByte)

  // ── Shared run helpers (referenced from the staged bodies) ──────────────────

  // ZIO's unsafe-run entry point, wrapping each ZIO benchmark's effect.
  def runZio[A](effect: zio.ZIO[Any, Throwable, A]): A =
    zio.Unsafe.unsafe: unsafe ?=>
      zio.Runtime.default.unsafe.run(effect).getOrThrow()

  // A fixed-capacity `Buffering`, for the block-size sweep.
  def buffering(n: Int): Buffering = new Buffering:
    def capacity(substrate: Substrate): Int = n
    def window: Int = 4

  // Int rather than Long: ZIO's `take`/`drop` are `Int`-counted, and both values
  // fit; Soundness's and FS2's `Long`-counted versions widen automatically.
  val dropBytes: Int = 65536
  val takeBytes: Int = 2*1024*1024

  def run(): Unit =
    val bench = Bench()
    val stress = Stress()
    val constrained = Stress(heap = t"128m")
    val gated = Stress(heap = t"2g", cpus = 4)
    val profile = Profile()
    val size = input.length*Byte
    val textSize = textData.length*Byte

    // Example 1: gzip compression (drain, count output bytes).
    suite(m"Gzip compression (4 MB)"):
      bench(m"Soundness  Stream.compress[Gzip]")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
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
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
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

            val buffer = new Array[Byte](65536)
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
        ( target = 1*Second, operationSize = textSize, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.textData.stream.via(summon[CharDecoder]).memoize.s.length }

      // The memoize row above concatenates the full 5 MB Text; this row counts
      // chars per window, the same aggregation shape as the FS2/ZIO rows.
      bench(m"Soundness  via(CharDecoder) fold")(target = 1*Second, operationSize = textSize):
        '{
            var total = 0L

            turbulence.Benchmarks.textData.stream.via(summon[CharDecoder])
            . sweep((_, _, count) => total += count)

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

    // Example 3: byte checksum fold. The Soundness fold runs over the raw window
    // with no per-element boxing; FS2/ZIO/Kyo box each byte as it flows.
    suite(m"Byte checksum fold (4 MB)"):
      bench(m"Soundness  sweep")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{
            var total = 0L
            turbulence.Benchmarks.input.stream.sweep: (storage, start, count) =>
              val arr = storage.asInstanceOf[Array[Byte]]
              var i = start
              while i < start + count do { total += (arr(i) & 0xff); i += 1 }
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
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
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
        ( target = 1*Second, operationSize = textSize, baseline = Baseline(compare = Min) ):
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
        ( target = 1*Second, operationSize = textSize, baseline = Baseline(compare = Min) ):
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
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
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

            val scratch = new Array[Byte](65536)
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
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{
            turbulence.Benchmarks.aesKey.expose:
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

            val scratch = new Array[Byte](65536)
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
        ( target = 1*Second, operationSize = textSize, baseline = Baseline(compare = Min) ):
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
        ( target = 1*Second, operationSize = textSize, baseline = Baseline(compare = Min) ):
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
    // the `drop`/`take`/`fold` kernel operators with transcoding. `drop`/`take`
    // are chunk-aware byte counts in all three libraries; the terminal count
    // folds over whole windows.
    suite(m"Chained: drop -> transcode -> take -> count (4 MB)"):
      bench(m"Soundness  drop.dec.enc.take.fold")
        ( target = 1*Second, operationSize = textSize, baseline = Baseline(compare = Min) ):
        '{
            turbulence.Benchmarks.textData.stream.drop(turbulence.Benchmarks.dropBytes)
            . via(summon[CharDecoder]).via(summon[CharEncoder])
            . take(turbulence.Benchmarks.takeBytes)
            . fold(0L)((total, _, _, count) => total + count)
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
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
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
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
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
    // drives the JCE cipher over the legacy `LazyList` view. ZIO/FS2/Kyo have no
    // native block cipher, so only the JDK reference is shown.
    suite(m"AES-256-CBC encrypt (4 MB)"):
      bench(m"Soundness  enigmatic encryptStream")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{
            turbulence.Benchmarks.aesKey.expose:
              LazyList(turbulence.Benchmarks.input).encrypt(InitializationVector.random)
              . map(_.length.toLong).sum
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
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.inputChunks.iterator.stream.memoize.length }

      bench(m"Soundness  read[Data]")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.inputChunks.read[Data].length }

      bench(m"Soundness  read[Text] (decode)")(target = 1*Second, operationSize = textSize):
        '{ turbulence.Benchmarks.textData.read[Text].s.length }

    // Example G: `Buffering` block-size sensitivity for the gzip staging buffer.
    suite(m"Buffering block-size sweep: gzip (4 MB)"):
      bench(m"block 4 KiB (standard)")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
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
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{
            val cursor = Cursor[Data](turbulence.Benchmarks.input)
            var total = 0L
            while !cursor.finished do
              total += cursor.peek.asInt
              cursor.next()
            total
        }

      bench(m"Raw  Array[Byte] loop")(target = 1*Second, operationSize = size):
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
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{
            val out = new java.io.ByteArrayOutputStream(turbulence.Benchmarks.input.length)
            turbulence.Benchmarks.inputChunks.writeTo(out)
            out.size
        }

      bench(m"Raw  OutputStream.write")(target = 1*Second, operationSize = size):
        '{
            val out = new java.io.ByteArrayOutputStream(turbulence.Benchmarks.input.length)
            turbulence.Benchmarks.inputChunks.foreach(chunk => out.write(chunk.asInstanceOf[Array[Byte]]))
            out.size
        }

    // Example J: hex / base32 encode (cost by base), with the JDK `HexFormat` as
    // a hex reference. Base32 has no common JDK/FS2 counterpart.
    suite(m"Hex / Base32 encode (4 MB)"):
      bench(m"Soundness  serialize[Hex]")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.input.serialize[Hex].s.length }

      bench(m"JDK  HexFormat")(target = 1*Second, operationSize = size):
        '{ java.util.HexFormat.of.formatHex(turbulence.Benchmarks.inputArray).length }

      bench(m"Soundness  serialize[Base32]")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.input.serialize[Base32].s.length }

    // Example K: `pump` pump (pull -> push OutputStream sink) vs `memoize`.
    suite(m"pump pump vs memoize (4 MB)"):
      bench(m"Soundness  Stream.memoize")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
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
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{
            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.foreach(intake.put)
              intake.finish())
            var total = 0L
            stream.sweep((_, _, count) => total += count)
            producer.join()
            total
        }

      bench(m"JDK  ArrayBlockingQueue")(target = 1*Second, operationSize = size):
        '{
            val queue = new java.util.concurrent.ArrayBlockingQueue[AnyRef](8)
            val end = new Object
            val producer = new Thread(() =>
              turbulence.Benchmarks.inputChunks.foreach(chunk => queue.put(chunk.asInstanceOf[AnyRef]))
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
                turbulence.Benchmarks.inputChunks.foldLeft(IO.unit): (io, chunk) =>
                  io *> channel.send(fs2.Chunk.array(chunk.asInstanceOf[Array[Byte]])).void
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
                  (turbulence.Benchmarks.inputChunks.map(c => Chunk.fromArray(c.asInstanceOf[Array[Byte]])))
              for
                queue <- Queue.bounded[Take[Nothing, Chunk[Byte]]](8)
                _     <- source.runIntoQueue(queue).fork
                total <- ZStream.fromQueue(queue).flattenTake.runFold(0L)((acc, c) => acc + c.size)
              yield total
        }

    // Example M: `Confluence` fan-in — merge four streams. A stable in-memory
    // source's window is shared by reference, exactly as the references pass
    // immutable chunks.
    suite(m"Confluence fan-in: merge 4 streams (4 MB)"):
      bench(m"Soundness  Confluence")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{
            supervise:
              val merged = Confluence(turbulence.Benchmarks.quarters.map(q => q.stream)*)
              var total = 0L
              merged.sweep((_, _, count) => total += count)
              total
        }

      bench(m"FS2  parJoinUnbounded")(target = 1*Second, operationSize = size):
        '{
            import cats.effect.unsafe.implicits.global
            import cats.effect.IO
            val streams =
              turbulence.Benchmarks.quarters.map: q =>
                fs2.Stream.chunk(fs2.Chunk.array(q.asInstanceOf[Array[Byte]])).covary[IO]
            fs2.Stream.emits(streams).parJoinUnbounded.compile.count.unsafeRunSync()
        }

      bench(m"ZIO  mergeAllUnbounded")(target = 1*Second, operationSize = size):
        '{
            turbulence.Benchmarks.runZio:
              import zio.*, zio.stream.*
              val streams =
                turbulence.Benchmarks.quarters.map(q => ZStream.fromChunk(Chunk.fromArray(q.asInstanceOf[Array[Byte]])))
              ZStream.mergeAllUnbounded()(streams*).runCount
        }

    // Example N: `Divergence` fan-out — broadcast to three consumers. The source
    // chunk is shared read-only between subscribers, so it too passes without
    // copying; the residual gap on fan-out is the thread-vs-fiber wakeup of the
    // subscriber consumers.
    suite(m"Divergence fan-out: broadcast to 3 (4 MB in, 12 MB consumed)"):
      bench(m"Soundness  Divergence")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{
            supervise:
              val subscribers = Divergence(turbulence.Benchmarks.input.stream, 3)
              val tasks = subscribers.map: subscriber =>
                async:
                  var total = 0L
                  subscriber.sweep((_, _, count) => total += count)
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

    // Example O: stress rows — the memory profile of the cross-thread hand-off
    // pipeline from Example L, run as 16 concurrent pipelines for a fixed
    // wall-clock window. The throughput rows above measure speed; these measure
    // the axis the bounded-buffer design targets: allocation per pipeline, peak
    // heap, and a retained live set that stays flat under concurrency.
    suite(m"Stress: cross-thread hand-off memory (4 MB in 64 KiB chunks, N=16)"):
      import threading.platformThreading

      stress(m"Soundness  Conduit")(target = 2*Second, concurrency = 16, baseline = Baseline()):
        '{
            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.foreach(intake.put)
              intake.finish())
            var total = 0L
            stream.sweep((_, _, count) => total += count)
            producer.join()
            total
        }

      stress(m"FS2  Channel.bounded")(target = 2*Second, concurrency = 16):
        '{
            import cats.effect.unsafe.implicits.global
            import cats.effect.IO
            val program = fs2.concurrent.Channel.bounded[IO, fs2.Chunk[Byte]](8).flatMap: channel =>
              val produce =
                turbulence.Benchmarks.inputChunks.foldLeft(IO.unit): (io, chunk) =>
                  io *> channel.send(fs2.Chunk.array(chunk.asInstanceOf[Array[Byte]])).void
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
                  (turbulence.Benchmarks.inputChunks.map(c => Chunk.fromArray(c.asInstanceOf[Array[Byte]])))
              for
                queue <- Queue.bounded[Take[Nothing, Chunk[Byte]]](8)
                _     <- source.runIntoQueue(queue).fork
                total <- ZStream.fromQueue(queue).flattenTake.runFold(0L)((acc, c) => acc + c.size)
              yield total
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
              turbulence.Benchmarks.inputChunks.foreach(intake.put)
              intake.finish())
            var total = 0L
            stream.sweep((_, _, count) => total += count)
            producer.join()
            total
        }

      constrained(m"FS2  Channel.bounded")(target = 1*Second, sweep = 64):
        '{
            import cats.effect.unsafe.implicits.global
            import cats.effect.IO
            val program = fs2.concurrent.Channel.bounded[IO, fs2.Chunk[Byte]](8).flatMap: channel =>
              val produce =
                turbulence.Benchmarks.inputChunks.foldLeft(IO.unit): (io, chunk) =>
                  io *> channel.send(fs2.Chunk.array(chunk.asInstanceOf[Array[Byte]])).void
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
                  (turbulence.Benchmarks.inputChunks.map(c => Chunk.fromArray(c.asInstanceOf[Array[Byte]])))
              for
                queue <- Queue.bounded[Take[Nothing, Chunk[Byte]]](8)
                _     <- source.runIntoQueue(queue).fork
                total <- ZStream.fromQueue(queue).flattenTake.runFold(0L)((acc, c) => acc + c.size)
              yield total
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
        ( target = 2*Second, concurrency = 8, baseline = Baseline() ):
        '{
            var total = 0L

            turbulence.Benchmarks.gzippedInput.stream.decompress[Gzip]
            . sweep((_, _, count) => total += count)

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
        ( target = 2*Second, concurrency = 8, baseline = Baseline() ):
        '{
            var total = 0L

            turbulence.Benchmarks.textData.stream.via(summon[CharDecoder])
            . sweep((_, _, count) => total += count)

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
                turbulence.Benchmarks.inputChunks.foreach(intake.put)
                intake.finish())
              var total = 0L
              stream.sweep((_, _, count) => total += count)
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
                  turbulence.Benchmarks.inputChunks.foldLeft(IO.unit): (io, chunk) =>
                    io *> channel.send(fs2.Chunk.array(chunk.asInstanceOf[Array[Byte]])).void
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
                    (turbulence.Benchmarks.inputChunks.map(c => Chunk.fromArray(c.asInstanceOf[Array[Byte]])))
                for
                  queue <- Queue.bounded[Take[Nothing, Chunk[Byte]]](8)
                  _     <- source.runIntoQueue(queue).fork
                  total <- ZStream.fromQueue(queue).flattenTake.runFold(0L)((acc, c) => acc + c.size)
                yield total
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
              turbulence.Benchmarks.inputChunks.foreach(intake.put)
              intake.finish())
            var total = 0L
            stream.sweep((_, _, count) => total += count)
            producer.join()
            total
        }

    // Example T: profiles — where the time actually goes in the two pipelines the
    // stress suites measure. Each renders as a histogram of the hottest methods
    // (self time, from JFR execution samples), coloured by package.
    suite(m"Profile: pipeline hotspots"):
      profile(m"Conduit hand-off (4 MB in 64 KiB chunks)")(target = 5*Second):
        '{
            val (intake, stream) = Conduit[Data]()
            val producer = Thread.ofVirtual.start(() =>
              turbulence.Benchmarks.inputChunks.foreach(intake.put)
              intake.finish())
            var total = 0L
            stream.sweep((_, _, count) => total += count)
            producer.join()
            total
        }

      profile(m"Stream.decompress[Gzip] (4 MB out)")(target = 5*Second):
        '{
            var total = 0L

            turbulence.Benchmarks.gzippedInput.stream.decompress[Gzip]
            . sweep((_, _, count) => total += count)

            total
        }
