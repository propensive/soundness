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
import parasite.*, threading.platformThreading, probates.panicProbate
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
  // The same 4 MB split into 64 KiB chunks, so aggregation/write loops iterate
  // (a single in-memory chunk would let `read[Data]` fold to an identity).
  lazy val inputChunks: LazyList[Data] =
    LazyList.from((0 until input.length by 65536).map: offset =>
      input.slice(offset, (offset + 65536).min(input.length)))

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
  lazy val gzippedText: Data = Stream(textData).compress[Gzip].memoize
  lazy val gzippedTextArray: Array[Byte] = gzippedText.asInstanceOf[Array[Byte]]

  // AES-256 key + a fixed key/IV for the JDK reference, generated/derived once.
  lazy val aesKey: SymmetricKey[Aes[256] over Cbc against Pkcs7] =
    SymmetricKey.generate[Aes[256] over Cbc against Pkcs7]()
  lazy val jdkKeyBytes: Array[Byte] = Array.tabulate(32)(i => (i*7 + 1).toByte)
  lazy val jdkIvBytes:  Array[Byte] = Array.tabulate(16)(i => (i*13 + 3).toByte)

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

  // ── Chained example A: gzip compress -> decompress roundtrip ────────────────

  def roundtripSoundness: Int = Stream(input).compress[Gzip].decompress[Gzip].memoize.length

  def roundtripFs2: Long =
    import cats.effect.unsafe.implicits.global
    val comp = fs2.compression.Compression.forSync[cats.effect.IO]
    fs2.Stream.chunk(fs2.Chunk.array(inputArray)).covary[cats.effect.IO]
    . through(comp.gzip()).through(comp.gunzip()).flatMap(_.content)
    . compile.count.unsafeRunSync()

  def roundtripZio: Long =
    runZio:
      zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(inputArray))
      . via(zio.stream.ZPipeline.gzip()).via(zio.stream.ZPipeline.gunzip())
      . runCount

  // ── Chained example B: UTF-8 decode -> re-encode transcode roundtrip ────────

  def transcodeSoundness: Int =
    Stream(textData).through(summon[CharDecoder]).through(summon[CharEncoder]).memoize.length

  def transcodeFs2: Long =
    import cats.effect.unsafe.implicits.global
    fs2.Stream.chunk(fs2.Chunk.array(textArray)).covary[cats.effect.IO]
    . through(fs2.text.utf8.decode).through(fs2.text.utf8.encode)
    . compile.count.unsafeRunSync()

  def transcodeZio: Long =
    runZio:
      zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(textArray))
      . via(zio.stream.ZPipeline.utfDecode).via(zio.stream.ZPipeline.utf8Encode)
      . runCount

  // ── Chained example C: gunzip -> UTF-8 decode -> count characters ───────────

  def readGzipSoundness: Int =
    Stream(gzippedText).decompress[Gzip].through(summon[CharDecoder]).memoize.s.length

  def readGzipFs2: Int =
    import cats.effect.unsafe.implicits.global
    val comp = fs2.compression.Compression.forSync[cats.effect.IO]
    fs2.Stream.chunk(fs2.Chunk.array(gzippedTextArray)).covary[cats.effect.IO]
    . through(comp.gunzip()).flatMap(_.content)
    . through(fs2.text.utf8.decode).map(_.length).compile.fold(0)(_ + _).unsafeRunSync()

  def readGzipZio: Int =
    runZio:
      zio.stream.ZStream.fromChunk(zio.Chunk.fromArray(gzippedTextArray))
      . via(zio.stream.ZPipeline.gunzip()).via(zio.stream.ZPipeline.utfDecode)
      . map(_.length).runSum

  // ── Example D: Base64 encode ────────────────────────────────────────────────
  // Soundness `monotonous` base-N is a whole-value operation (no streaming duct
  // today); FS2 has a native streaming base64 pipe; the JDK is the universal
  // reference. ZIO/Kyo have no native base64.

  def base64Soundness: Int = input.serialize[Base64].s.length

  def base64Fs2: Int =
    import cats.effect.unsafe.implicits.global
    fs2.Stream.chunk(fs2.Chunk.array(inputArray)).covary[cats.effect.IO]
    . through(fs2.text.base64.encode).map(_.length).compile.fold(0)(_ + _).unsafeRunSync()

  def base64Jdk: Int = java.util.Base64.getEncoder.encodeToString(inputArray).length

  // ── Example E: AES-256-CBC encrypt ──────────────────────────────────────────
  // Soundness `enigmatic` streaming encryption drives the JCE cipher over the
  // legacy `LazyList` view (enigmatic is not yet on the streaming kernel), and
  // prepends the IV as the leading chunk. The JDK reference is the same cipher
  // with no framing. ZIO/FS2/Kyo have no native block cipher — they would wrap
  // the same `javax.crypto.Cipher`, so only the JDK reference is shown.

  def aesSoundness: Long =
    aesKey.expose:
      LazyList(input).encrypt(InitializationVector.random).map(_.length.toLong).sum

  def aesJdk: Int =
    val cipher = javax.crypto.Cipher.getInstance("AES/CBC/PKCS5Padding")
    cipher.init
      ( javax.crypto.Cipher.ENCRYPT_MODE,
        javax.crypto.spec.SecretKeySpec(jdkKeyBytes, "AES"),
        javax.crypto.spec.IvParameterSpec(jdkIvBytes) )
    cipher.doFinal(inputArray).length

  // ── Example J: hex / base32 encode (cost by base) ───────────────────────────
  // Soundness monotonous alphabets at 4, 5 and 6 bits per character
  // (hex/base32/base64), with the JDK HexFormat as a hex reference. Base32 has
  // no common JDK/FS2 counterpart, so it stands alone.

  def hexSoundness: Int = input.serialize[Hex].s.length
  def hexJdk: Int = java.util.HexFormat.of.formatHex(inputArray).length
  def base32Soundness: Int = input.serialize[Base32].s.length

  // ── Example K: flowTo pump (pull -> push) vs memoize ────────────────────────
  // `flowTo` pumps a pull `Stream` into a push `Intake` (here an OutputStream
  // sink); `memoize` collects into a value. Both drain the same 4 MB.

  def flowToSink: Int =
    val out = new java.io.ByteArrayOutputStream(input.length)
    Stream(input).flowTo(summon[java.io.OutputStream is Sink by Data over Credit].intake(out))
    out.size

  def memoizeSingle: Int = Stream(input).memoize.length

  // ── Example L: Conduit cross-thread hand-off ────────────────────────────────
  // A bounded single-producer/single-consumer boundary: produce the 4 MB in
  // 64 KiB chunks on one thread/fiber, consume on another. Soundness `Conduit`
  // against the JDK `ArrayBlockingQueue`, FS2 `Channel` and ZIO `Queue`.
  //
  // Two asymmetries dominate and are the point of the comparison:
  //   * Soundness `Conduit` COPIES data into fixed kernel-block buffers (its
  //     bounded-memory guarantee) and re-chunks the 64 KiB inputs to the block
  //     size, so it performs many small copying hand-offs; the queues pass the
  //     chunk REFERENCES with zero copy.
  //   * Soundness and the JDK use a real OS `Thread` producer; FS2/ZIO run the
  //     producer as a fiber on the same carrier thread, avoiding OS
  //     context-switch cost per hand-off.

  def conduitSoundness: Long =
    val (intake, stream) = Conduit[Data]()
    val producer = new Thread(() =>
      inputChunks.foreach(intake.put)
      intake.finish())
    producer.start()
    var total = 0L
    stream.foreachWindow((_, _, count) => total += count)
    producer.join()
    total

  def conduitJdk: Long =
    val queue = new java.util.concurrent.ArrayBlockingQueue[AnyRef](8)
    val end = new Object
    val producer = new Thread(() =>
      inputChunks.foreach(chunk => queue.put(chunk.asInstanceOf[AnyRef]))
      queue.put(end))
    producer.start()
    var total = 0L
    var running = true
    while running do
      val item = queue.take()
      if item eq end then running = false else total += item.asInstanceOf[Data].length
    producer.join()
    total

  def conduitFs2: Long =
    import cats.effect.unsafe.implicits.global
    import cats.effect.IO, cats.syntax.all.*
    val program = fs2.concurrent.Channel.bounded[IO, fs2.Chunk[Byte]](8).flatMap: channel =>
      val produce =
        inputChunks.foldLeft(IO.unit): (io, chunk) =>
          io *> channel.send(fs2.Chunk.array(chunk.asInstanceOf[Array[Byte]])).void
        *> channel.close.void
      produce.start *> channel.stream.compile.fold(0L)((acc, chunk) => acc + chunk.size)
    program.unsafeRunSync()

  def conduitZio: Long =
    runZio:
      import zio.*, zio.stream.*
      val source =
        ZStream.fromIterable(inputChunks.map(c => Chunk.fromArray(c.asInstanceOf[Array[Byte]])))
      for
        queue <- Queue.bounded[Take[Nothing, Chunk[Byte]]](8)
        _     <- source.runIntoQueue(queue).fork
        total <- ZStream.fromQueue(queue).flattenTake.runFold(0L)((acc, c) => acc + c.size)
      yield total

  // ── Example F: public `read` typeclass path vs the bare kernel ──────────────
  // `read[Data]`/`read[Text]` go through the `Readable`/`Source`/`Aggregable`
  // typeclass machinery; `memoize` is the raw kernel drain. The gap is what the
  // high-level public API costs over the endpoint directly.

  def memoizeChunked: Int = Stream(inputChunks.iterator).memoize.length
  def readChunked: Int = inputChunks.read[Data].length
  def readText: Int = textData.read[Text].s.length

  // ── Example G: `Buffering` block-size sensitivity (gzip staging buffer) ─────
  // Each gzip duct stages input in a `Buffering.capacity`-sized buffer; this
  // sweeps that size to show its effect on streaming-compression throughput.

  def buffering(n: Int): Buffering = new Buffering:
    def capacity(substrate: Substrate): Int = n
    def window: Int = 4

  def gzipBlock512: Int   = Stream(input).compress[Gzip](using summon, buffering(512)).memoize.length
  def gzipBlock4k: Int    = Stream(input).compress[Gzip](using summon, buffering(4096)).memoize.length
  def gzipBlock64k: Int   = Stream(input).compress[Gzip](using summon, buffering(65536)).memoize.length

  // ── Example H: `Cursor` parser pull-loop vs a raw array loop ────────────────
  // The hot path jacinta/honeycomb run: pull every byte through the `Cursor`
  // abstraction (`peek`/`next`), compared against a bare `Array[Byte]` scan.

  def cursorSum: Long =
    val cursor = Cursor[Data](input)
    var total = 0L
    while !cursor.finished do
      total += cursor.peek.asInt
      cursor.next()
    total

  def rawArraySum: Long =
    var total = 0L
    var i = 0
    while i < inputArray.length do { total += (inputArray(i) & 0xff); i += 1 }
    total

  // ── Example I: `writeTo` sink path vs a raw OutputStream write ──────────────

  def writeToSink: Int =
    val out = new java.io.ByteArrayOutputStream(input.length)
    inputChunks.writeTo(out)
    out.size

  def rawWrite: Int =
    val out = new java.io.ByteArrayOutputStream(input.length)
    inputChunks.foreach(chunk => out.write(chunk.asInstanceOf[Array[Byte]]))
    out.size

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

    // The chained pipelines must do equivalent work: both roundtrips are the
    // identity on length, and the three gunzip->decode impls must agree on the
    // decoded character count.
    test(m"chained pipelines agree"):
      val roundtripOk = roundtripSoundness == input.length
      val transcodeOk = transcodeSoundness == textData.length
      val readOk = readGzipSoundness == readGzipFs2 && readGzipFs2 == readGzipZio
      val base64Ok = base64Soundness == base64Jdk && base64Jdk == base64Fs2
      val readOk2 = readChunked == input.length && memoizeChunked == input.length
      val cursorOk = cursorSum == rawArraySum && cursorSum == checksumSoundness
      val writeOk = writeToSink == input.length && rawWrite == input.length
      val hexOk = hexSoundness == hexJdk && hexSoundness == input.length*2
      val flowOk = flowToSink == input.length && memoizeSingle == input.length
      val n = input.length.toLong
      val conduitOk =
        conduitSoundness == n && conduitJdk == n && conduitFs2 == n && conduitZio == n
      ( roundtripOk, transcodeOk, readOk, base64Ok, readOk2, cursorOk, writeOk, hexOk, flowOk,
        conduitOk )
    . assert(_ == (true, true, true, true, true, true, true, true, true, true))

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

    suite(m"Chained: gzip -> gunzip roundtrip (4 MB)"):
      bench(m"Soundness  compress[Gzip].decompress[Gzip]")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.roundtripSoundness }

      bench(m"FS2  gzip.gunzip")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.roundtripFs2 }

      bench(m"ZIO  gzip.gunzip")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.roundtripZio }

    suite(m"Chained: UTF-8 decode -> encode transcode (4 MB)"):
      bench(m"Soundness  through(dec).through(enc)")
        ( target = 1*Second, operationSize = textSize, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.transcodeSoundness }

      bench(m"FS2  utf8.decode.encode")(target = 1*Second, operationSize = textSize):
        '{ turbulence.Benchmarks.transcodeFs2 }

      bench(m"ZIO  utfDecode.utf8Encode")(target = 1*Second, operationSize = textSize):
        '{ turbulence.Benchmarks.transcodeZio }

    suite(m"Chained: gunzip -> UTF-8 decode -> count (gzipped text)"):
      bench(m"Soundness  decompress.through(dec)")
        ( target = 1*Second, operationSize = textSize, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.readGzipSoundness }

      bench(m"FS2  gunzip.utf8.decode")(target = 1*Second, operationSize = textSize):
        '{ turbulence.Benchmarks.readGzipFs2 }

      bench(m"ZIO  gunzip.utfDecode")(target = 1*Second, operationSize = textSize):
        '{ turbulence.Benchmarks.readGzipZio }

    suite(m"Base64 encode (4 MB)"):
      bench(m"Soundness  serialize[Base64]")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.base64Soundness }

      bench(m"FS2  text.base64.encode")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.base64Fs2 }

      bench(m"JDK  java.util.Base64")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.base64Jdk }

    suite(m"AES-256-CBC encrypt (4 MB)"):
      bench(m"Soundness  enigmatic encryptStream")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.aesSoundness }

      bench(m"JDK  javax.crypto.Cipher")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.aesJdk }

    suite(m"Public read vs kernel memoize (4 MB)"):
      bench(m"Soundness  Stream.memoize (kernel)")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.memoizeChunked }

      bench(m"Soundness  read[Data]")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.readChunked }

      bench(m"Soundness  read[Text] (decode)")(target = 1*Second, operationSize = textSize):
        '{ turbulence.Benchmarks.readText }

    suite(m"Buffering block-size sweep: gzip (4 MB)"):
      bench(m"block 4 KiB (standard)")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.gzipBlock4k }

      bench(m"block 512 B")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.gzipBlock512 }

      bench(m"block 64 KiB")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.gzipBlock64k }

    suite(m"Cursor pull-loop vs raw array scan (4 MB)"):
      bench(m"Soundness  Cursor peek/next")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.cursorSum }

      bench(m"Raw  Array[Byte] loop")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.rawArraySum }

    suite(m"writeTo sink vs raw OutputStream write (4 MB)"):
      bench(m"Soundness  writeTo")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.writeToSink }

      bench(m"Raw  OutputStream.write")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.rawWrite }

    suite(m"Hex / Base32 encode (4 MB)"):
      bench(m"Soundness  serialize[Hex]")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.hexSoundness }

      bench(m"JDK  HexFormat")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.hexJdk }

      bench(m"Soundness  serialize[Base32]")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.base32Soundness }

    suite(m"flowTo pump vs memoize (4 MB)"):
      bench(m"Soundness  Stream.memoize")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.memoizeSingle }

      bench(m"Soundness  flowTo(sink)")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.flowToSink }

    suite(m"Conduit cross-thread hand-off (4 MB in 64 KiB chunks)"):
      bench(m"Soundness  Conduit")
        ( target = 1*Second, operationSize = size, baseline = Baseline(compare = Min) ):
        '{ turbulence.Benchmarks.conduitSoundness }

      bench(m"JDK  ArrayBlockingQueue")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.conduitJdk }

      bench(m"FS2  Channel.bounded")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.conduitFs2 }

      bench(m"ZIO  Queue.bounded")(target = 1*Second, operationSize = size):
        '{ turbulence.Benchmarks.conduitZio }
