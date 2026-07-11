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
import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import hieroglyph.*, charDecoders.utf8Decoder, textSanitizers.strictSanitizer
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
// effect-based streaming libraries ZIO-Streams, FS2 and Kyo. Three operations,
// three cost profiles: a heavy CPU transform (gzip), a stateful transcode stage
// (UTF-8 decode) and bare pipeline overhead (byte checksum fold).
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
//     `Stream(value)` copies once at construction. Kyo is fed an `ArraySeq`
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

  // ~4 MB of UTF-8 text with multi-byte characters, so the decode exercises the
  // cross-chunk continuation path in every library.
  lazy val textData: Data =
    val unit = t"The quick brown fox — jümps over the lazy dog. café ☕ 数据 🚀\n"
    val builder = new java.lang.StringBuilder(4 << 20)
    while builder.length < (4 << 20) do builder.append(unit.s)
    Data(builder.toString.getBytes("UTF-8").nn*)
  lazy val textArray: Array[Byte] = textData.asInstanceOf[Array[Byte]]

  // ── ZIO / Kyo run entry points ──────────────────────────────────────────────

  def runZio[A](effect: zio.ZIO[Any, Throwable, A]): A =
    zio.Unsafe.unsafe: unsafe ?=>
      zio.Runtime.default.unsafe.run(effect).getOrThrow()

  // ── Example 1: gzip compression (drain, count output bytes) ─────────────────

  def gzipSoundness: Int = Stream(input).compress[Gzip].memoize.length

  def gzipFs2: Long =
    import cats.effect.unsafe.implicits.global
    fs2.Stream.chunk(fs2.Chunk.array(inputArray)).covary[cats.effect.IO]
    . through(fs2.compression.Compression.forSync[cats.effect.IO].gzip())
    . compile.count.unsafeRunSync()

  def gzipZio: Long =
    runZio:
      zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(inputArray))
      . via(zio.stream.ZPipeline.gzip())
      . runCount

  // ── Example 2: UTF-8 decode (count decoded characters) ──────────────────────

  def utf8Soundness: Int = Stream(textData).through(summon[CharDecoder]).memoize.s.length

  def utf8Fs2: Int =
    import cats.effect.unsafe.implicits.global
    fs2.Stream.chunk(fs2.Chunk.array(textArray)).covary[cats.effect.IO]
    . through(fs2.text.utf8.decode).map(_.length).compile.fold(0)(_ + _).unsafeRunSync()

  def utf8Zio: Int =
    runZio:
      zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(textArray))
      . via(zio.stream.ZPipeline.utfDecode).map(_.length).runSum

  // ── Example 3: byte checksum fold ───────────────────────────────────────────

  def checksumSoundness: Long =
    var total = 0L
    Stream(input).foreachWindow: (storage, start, count) =>
      val arr = storage.asInstanceOf[Array[Byte]]
      var i = start
      while i < start + count do { total += (arr(i) & 0xff); i += 1 }
    total

  def checksumFs2: Long =
    import cats.effect.unsafe.implicits.global
    fs2.Stream.chunk(fs2.Chunk.array(inputArray)).covary[cats.effect.IO]
    . compile.fold(0L)((acc, b) => acc + (b & 0xff)).unsafeRunSync()

  def checksumZio: Long =
    runZio:
      zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(inputArray))
      . runFold(0L)((acc, b) => acc + (b & 0xff))

  def checksumKyo: Long =
    import kyo.*
    Stream.init(inputSeq).fold(0L)((acc, b) => acc + (b & 0xff)).eval

  def run(): Unit =
    val bench = Bench()
    val size = input.length*Byte
    val textSize = textData.length*Byte

    // Sanity: the checksum must be identical across all four implementations
    // (deterministic sum), confirming they fold the same bytes.
    test(m"checksum agreement"):
      ( checksumSoundness, checksumFs2, checksumZio, checksumKyo )
    . assert: (s, f, z, k) =>
        s == f && f == z && z == k

    suite(m"Gzip compression (4 MB)"):
      bench(m"Soundness  Stream.compress[Gzip]")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.gzipSoundness }

      bench(m"FS2  Compression[IO].gzip")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.gzipFs2 }

      bench(m"ZIO  ZPipeline.gzip")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.gzipZio }

    suite(m"UTF-8 decode (4 MB)"):
      bench(m"Soundness  through(CharDecoder)")
        ( target = 1*Second, operationSize = textSize, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.utf8Soundness }

      bench(m"FS2  text.utf8.decode")(target = 1*Second, operationSize = textSize):
        '{ turbulence.Benchmarks.utf8Fs2 }

      bench(m"ZIO  ZPipeline.utfDecode")(target = 1*Second, operationSize = textSize):
        '{ turbulence.Benchmarks.utf8Zio }

    suite(m"Byte checksum fold (4 MB)"):
      bench(m"Soundness  foreachWindow")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.checksumSoundness }

      bench(m"FS2  compile.fold")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.checksumFs2 }

      bench(m"ZIO  runFold")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.checksumZio }

      bench(m"Kyo  Stream.fold")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.checksumKyo }
