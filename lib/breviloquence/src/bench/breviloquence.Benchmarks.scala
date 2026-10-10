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
package breviloquence

import ambience.*, environments.javaBaseEnvironment, systems.javaBaseSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import denominative.*
import prepositional.*
import probably.*
import proscenium.*
import quantitative.*
import rudiments.*
import sedentary.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import turbulence.*
import vacuous.*

object Benchmarks extends Suite(m"Breviloquence CBOR parser benchmarks"):
  sealed trait Information extends Dimension
  sealed trait Bytes[Power <: Nat] extends Units[Power, Information]
  val Byte: MetricUnit[Bytes[1]] = MetricUnit(1.0)

  given byteDesignation: Designation[Bytes[1]] = () => t"B"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice
  given prefixes:        Prefixes               = Prefixes(List(Kilo, Mega, Giga, Tera))

  // Jackson's CBOR ObjectMapper — shared across iterations because construction
  // is expensive and intended to be amortised in production.
  val jacksonMapper: com.fasterxml.jackson.databind.ObjectMapper =
    new com.fasterxml.jackson.databind.ObjectMapper(
      new com.fasterxml.jackson.dataformat.cbor.CBORFactory())

  // Helper to build CBOR bytes by hand for the benchmark corpora. Uses the
  // canonical encoder so the inputs are well-formed and deterministic.
  def encode(ast: Cbor.Ast): Array[Byte]^{} = Cbor.Ast.encodable.encoded(ast)

  // Corpus 1: a small object with three string-keyed entries (id, name, active).
  // Roughly 30 bytes — exercises the head-byte fast path for short strings and
  // a small definite-length map.
  lazy val cborBytes1: Array[Byte]^{} =
    val keys = Array[Any]("id", "name", "active")
    val values = Array[Any](42L, "Alice", true)
    encode(Cbor.Ast.map(keys, values))

  // Corpus 2: 100 user records — typical "array of records" pattern with five
  // repeated keys per element.
  lazy val cborBytes2: Array[Byte]^{} =
    val records = (0 until 100).map: index =>
      val keys = Array[Any]("id", "username", "email", "active", "role")
      val active = (index&1) == 0
      val role = if index%10 == 0 then "admin" else "user"
      val values = Array[Any](index.toLong, s"user$index", s"user$index@example.com", active, role)
      Cbor.Ast.map(keys, values).asInstanceOf[Any]
    encode(Cbor.Ast.map(Array[Any]("users"), Array[Any](Cbor.Ast.array(Array.from(records)))))

  // Corpus 3: 500 log entries with six keys each — larger throughput target,
  // dominated by short-string parsing and small-integer head bytes.
  lazy val cborBytes3: Array[Byte]^{} =
    val levels = scala.Array("info", "debug", "warn", "error")
    val services = scala.Array("auth", "api", "db", "cache", "worker")
    val records = (0 until 500).map: index =>
      val keys = Array[Any]("timestamp", "level", "service", "requestId", "userId", "message")
      val ts = 1700000000L + index
      val level = levels(index & 3)
      val service = services(index % 5)
      val userId = 1000L + (index % 50)
      val values = Array[Any](ts, level, service, s"req-$index", userId, s"event $index processed")
      Cbor.Ast.map(keys, values).asInstanceOf[Any]
    encode(Cbor.Ast.map(Array[Any]("logs"), Array[Any](Cbor.Ast.array(Array.from(records)))))

  // Corpus 4: 1000 small integers — exercises the integer head-byte hot path
  // without string or map overhead.
  lazy val cborBytes4: Array[Byte]^{} =
    val items = Array.from((0 until 1000).map{ index => (index*37 + 1).toLong.asInstanceOf[Any] })
    encode(Cbor.Ast.array(items))

  // Corpus 5: 100 byte-string records — exercises major-type-2 (byte strings),
  // which JSON has no analog for.
  lazy val cborBytes5: Array[Byte]^{} =
    val records = (0 until 100).map: index =>
      val payload = new scala.Array[Byte](32)
      var j = 0
      while j < payload.length do { payload(j) = ((index + j) & 0xFF).toByte; j += 1 }
      payload.asInstanceOf[Array[Byte]^{}].asInstanceOf[Any]

    encode(Cbor.Ast.array(Array.from(records)))

  // Corpus 6: deeply nested structure (10-level wrapping) — stresses recursion
  // and the small-array head-byte path.
  lazy val cborBytes6: Array[Byte]^{} =
    var ast: Any = "deep"
    var index = 0
    while index < 10 do
      ast = Cbor.Ast.map(Array[Any](s"level$index"), Array[Any](ast))
      index += 1
    encode(ast.asInstanceOf[Cbor.Ast])

  // The log-entry record shared by corpora 3 and 7 and the stress document.
  def logEntry(index: Int): Cbor.Ast =
    val level = (index & 3) match
      case 0 => "info"
      case 1 => "debug"
      case 2 => "warn"
      case _ => "error"

    val service = (index % 5) match
      case 0 => "auth"
      case 1 => "api"
      case 2 => "db"
      case 3 => "cache"
      case _ => "worker"

    val keys = Array[Any]("timestamp", "level", "service", "requestId", "userId", "message")
    val ts = 1700000000L + index
    val userId = 1000L + (index % 50)
    val values =
      Array[Any](ts, level, service, s"req-$index", userId, s"event $index processed")
    Cbor.Ast.map(keys, values)

  // Corpus 7: 5000 log entries (~400 KiB) — large enough that a 4 KiB chunking yields ~100
  // chunks, so the chunked rows measure reading across many chunk boundaries rather than
  // the per-read fixed cost.
  lazy val cborBytes7: Array[Byte]^{} =
    val records = (0 until 5000).map(index => logEntry(index).asInstanceOf[Any])
    encode(Cbor.Ast.map(Array[Any]("logs"), Array[Any](Cbor.Ast.array(Array.from(records)))))

  // Corpus 7 split into 4 KiB chunks, as a transport would deliver it: a strict chain of
  // distinct segments for the Soundness rows, and the same segments as a `List` for the
  // rival rows, which read them through a `SequenceInputStream`.
  val ChunkSize: Int = 4096

  lazy val chunkList7: List[Data] =
    val length = cborBytes7.length
    List.tabulate((length + ChunkSize - 1)/ChunkSize): index =>
      val start = index*ChunkSize
      val end = (start + ChunkSize).min(length)
      cborBytes7.segment(start.z till end.z)

  lazy val chunks7: Chain[Data] = chunkList7.to[Chain]

  def inputStream(chunks: List[Data]): java.io.InputStream =
    val streams = new java.util.ArrayList[java.io.InputStream]()
    chunks.each: chunk =>
      streams.add(new java.io.ByteArrayInputStream(chunk.asInstanceOf[scala.Array[Byte]]))
    new java.io.SequenceInputStream(java.util.Collections.enumeration(streams))

  // Jackson's streaming factory, for the token-walk rows (no tree is built).
  val jacksonFactory: com.fasterxml.jackson.dataformat.cbor.CBORFactory =
    new com.fasterxml.jackson.dataformat.cbor.CBORFactory()

  // The stress document: `{"logs": [` … `]}` with the array indefinite-length, whose body is
  // one 64 KiB block of whole log-entry maps re-emitted `StressBlocks` times. A `def`, not a
  // `lazy val`, so each operation streams a fresh chain, and every cell of that chain refers
  // to the *same* block, so nothing retained by a chain head grows with the document: only
  // the parser's own buffering is measured. The block is cut from the encoding of a definite
  // array of the entries by dropping the array's three-byte head (`99 nn nn` for 256..65535
  // elements), which `run()` checks.
  val StressBlocks: Int = 1024

  lazy val stressBlock: Data =
    val count = 720
    val records = (0 until count).map(index => logEntry(index).asInstanceOf[Any])
    val array = encode(Cbor.Ast.array(Array.from(records)))
    array.segment((3).z till array.length.z)

  lazy val stressHead: Data =
    Array[Byte](0xA1.toByte, 0x64, 'l'.toByte, 'o'.toByte, 'g'.toByte, 's'.toByte, 0x9F.toByte)

  lazy val stressFoot: Data = Array[Byte](0xFF.toByte)

  def stressDocument: Chain[Data] =
    def repeat(n: Int): Chain[Data] =
      if n == 0 then Chain() else Chain.cons(stressBlock, repeat(n - 1))
    Chain(stressHead) #::: repeat(StressBlocks) #::: Chain(stressFoot)

  lazy val stressSize: Long = stressHead.length + StressBlocks.toLong*stressBlock.length + 1

  // Pre-converted to plain Array[Byte] for the comparison parsers (Jackson and
  // borer take Array[Byte]; the unwrapping cast is safe for read-only
  // benchmarks since neither parser mutates the input).
  lazy val raw1: scala.Array[Byte] = cborBytes1.asInstanceOf[scala.Array[Byte]]
  lazy val raw2: scala.Array[Byte] = cborBytes2.asInstanceOf[scala.Array[Byte]]
  lazy val raw3: scala.Array[Byte] = cborBytes3.asInstanceOf[scala.Array[Byte]]
  lazy val raw4: scala.Array[Byte] = cborBytes4.asInstanceOf[scala.Array[Byte]]
  lazy val raw5: scala.Array[Byte] = cborBytes5.asInstanceOf[scala.Array[Byte]]
  lazy val raw6: scala.Array[Byte] = cborBytes6.asInstanceOf[scala.Array[Byte]]

  def run(): Unit =
    val bench = Bench()
    val constrained = Stress(heap = t"512m")

    // The stress block must begin where the definite array's elements begin.
    val arrayHead = encode(Cbor.Ast.array(Array.from((0 until 720).map(index => logEntry(index).asInstanceOf[Any]))))
    if (arrayHead.readable(0) & 0xFF) != 0x99 then panic(m"stress block head is not a 2-byte-length array")

    val size1 = cborBytes1.length*Byte
    val size2 = cborBytes2.length*Byte
    val size3 = cborBytes3.length*Byte
    val size4 = cborBytes4.length*Byte
    val size5 = cborBytes5.length*Byte
    val size6 = cborBytes6.length*Byte
    val size7 = cborBytes7.length*Byte

    suite(m"Parse small object (3 fields)"):
      bench(m"Parse with Breviloquence")
        ( target = 1*Second, operationSize = size1 ):
        '{ Cbor.Ast.parse(breviloquence.Benchmarks.cborBytes1) }

      bench(m"Parse with Jackson")(target = 1*Second, operationSize = size1):
        '{ breviloquence.Benchmarks.jacksonMapper.readTree(breviloquence.Benchmarks.raw1).nn }

      bench(m"Parse with borer")(target = 1*Second, operationSize = size1):
        '{
            io.bullet.borer.Cbor.decode(breviloquence.Benchmarks.raw1)
            . to[io.bullet.borer.Dom.Element]
            . value
        }

    suite(m"Parse 100 user records"):
      bench(m"Parse with Breviloquence")
        ( target = 1*Second, operationSize = size2 ):
        '{ Cbor.Ast.parse(breviloquence.Benchmarks.cborBytes2) }

      bench(m"Parse directly with Breviloquence")(target = 1*Second, operationSize = size2):
        '{
            given BenchUsers is Cbor.Parsable = breviloquence.benchUsersParsable
            breviloquence.Benchmarks.cborBytes2.read[BenchUsers in Cbor]
        }

      bench(m"Parse with Jackson")(target = 1*Second, operationSize = size2):
        '{ breviloquence.Benchmarks.jacksonMapper.readTree(breviloquence.Benchmarks.raw2).nn }

      bench(m"Parse with borer")(target = 1*Second, operationSize = size2):
        '{
            io.bullet.borer.Cbor.decode(breviloquence.Benchmarks.raw2)
            . to[io.bullet.borer.Dom.Element]
            . value
        }

    suite(m"Parse 500 log entries"):
      bench(m"Parse with Breviloquence")
        ( target = 1*Second, operationSize = size3 ):
        '{ Cbor.Ast.parse(breviloquence.Benchmarks.cborBytes3) }

      bench(m"Parse directly with Breviloquence")(target = 1*Second, operationSize = size3):
        '{
            given Logs is Cbor.Parsable = breviloquence.logsParsable
            breviloquence.Benchmarks.cborBytes3.read[Logs in Cbor]
        }

      bench(m"Parse with Jackson")(target = 1*Second, operationSize = size3):
        '{ breviloquence.Benchmarks.jacksonMapper.readTree(breviloquence.Benchmarks.raw3).nn }

      bench(m"Parse with borer")(target = 1*Second, operationSize = size3):
        '{
            io.bullet.borer.Cbor.decode(breviloquence.Benchmarks.raw3)
            . to[io.bullet.borer.Dom.Element]
            . value
        }

    suite(m"Parse 1000 small integers"):
      bench(m"Parse with Breviloquence")
        ( target = 1*Second, operationSize = size4 ):
        '{ Cbor.Ast.parse(breviloquence.Benchmarks.cborBytes4) }

      bench(m"Parse with Jackson")(target = 1*Second, operationSize = size4):
        '{ breviloquence.Benchmarks.jacksonMapper.readTree(breviloquence.Benchmarks.raw4).nn }

      bench(m"Parse with borer")(target = 1*Second, operationSize = size4):
        '{
            io.bullet.borer.Cbor.decode(breviloquence.Benchmarks.raw4)
            . to[io.bullet.borer.Dom.Element]
            . value
        }

    suite(m"Parse 100 byte strings"):
      bench(m"Parse with Breviloquence")
        ( target = 1*Second, operationSize = size5 ):
        '{ Cbor.Ast.parse(breviloquence.Benchmarks.cborBytes5) }

      bench(m"Parse with Jackson")(target = 1*Second, operationSize = size5):
        '{ breviloquence.Benchmarks.jacksonMapper.readTree(breviloquence.Benchmarks.raw5).nn }

      bench(m"Parse with borer")(target = 1*Second, operationSize = size5):
        '{
            io.bullet.borer.Cbor.decode(breviloquence.Benchmarks.raw5)
            . to[io.bullet.borer.Dom.Element]
            . value
        }

    suite(m"Parse 10-level nested map"):
      bench(m"Parse with Breviloquence")
        ( target = 1*Second, operationSize = size6 ):
        '{ Cbor.Ast.parse(breviloquence.Benchmarks.cborBytes6) }

      bench(m"Parse with Jackson")(target = 1*Second, operationSize = size6):
        '{ breviloquence.Benchmarks.jacksonMapper.readTree(breviloquence.Benchmarks.raw6).nn }

      bench(m"Parse with borer")(target = 1*Second, operationSize = size6):
        '{
            io.bullet.borer.Cbor.decode(breviloquence.Benchmarks.raw6)
            . to[io.bullet.borer.Dom.Element]
            . value
        }

    // -------------------------------------------------------------------------
    // Chunked input: corpus 7 delivered as ~100 chunks of 4 KiB, the shape a
    // transport produces. The Soundness rows read the public `Chain` entry
    // points (AST, and direct through a generated `Cbor.Parsable`); the rivals
    // read the same chunks through an `InputStream`. Jackson's token walk builds
    // nothing, so it is the floor for a streaming read.
    // -------------------------------------------------------------------------

    suite(m"Parse 5000 log entries, chunked 4 KiB"):
      bench(m"Parse AST with Breviloquence")(target = 1*Second, operationSize = size7):
        '{ breviloquence.Benchmarks.chunks7.read[Cbor.Ast] }

      bench(m"Parse directly with Breviloquence")(target = 1*Second, operationSize = size7):
        '{
            given Logs is Cbor.Parsable = breviloquence.logsParsable
            breviloquence.Benchmarks.chunks7.read[Logs in Cbor]
        }

      bench(m"Count directly with Breviloquence")(target = 1*Second, operationSize = size7):
        '{
            given Long is Cbor.Parsable = breviloquence.countLogs
            breviloquence.Benchmarks.chunks7.read[Long in Cbor]
        }

      bench(m"Parse tree with Jackson")(target = 1*Second, operationSize = size7):
        '{
            val input = breviloquence.Benchmarks.inputStream(breviloquence.Benchmarks.chunkList7)
            breviloquence.Benchmarks.jacksonMapper.readTree(input).nn
        }

      bench(m"Walk tokens with Jackson")(target = 1*Second, operationSize = size7):
        '{
            val input = breviloquence.Benchmarks.inputStream(breviloquence.Benchmarks.chunkList7)
            val parser = breviloquence.Benchmarks.jacksonFactory.createParser(input).nn
            var count = 0L
            while parser.nextToken() != null do count += 1
            parser.close()
            count
        }

      bench(m"Parse with borer")(target = 1*Second, operationSize = size7):
        '{
            val input = breviloquence.Benchmarks.inputStream(breviloquence.Benchmarks.chunkList7)
            io.bullet.borer.Cbor.decode(input).to[io.bullet.borer.Dom.Element].value
        }

    // -------------------------------------------------------------------------
    // Bounded memory: a 64 MiB document streamed through the direct parser in a
    // pinned 512 MB heap. The counting consumer retains nothing, so the peak
    // heap and post-run live set are the parser's own: a parser that assembles
    // the input before decoding peaks at a multiple of the document size; a
    // streaming parser at a few chunks.
    // -------------------------------------------------------------------------

    suite(m"Stress: 64 MiB document streamed through the direct parser (512 MB heap)"):
      import parasite.threads.platformThreads

      constrained(m"Count directly with Breviloquence")(target = 5*Second):
        '{
            given Long is Cbor.Parsable = breviloquence.countLogs
            breviloquence.Benchmarks.stressDocument.read[Long in Cbor]
        }

      constrained(m"Walk tokens with Jackson")(target = 5*Second):
        '{
            val input = breviloquence.Benchmarks.inputStream(breviloquence.Benchmarks.stressDocument.to[List])
            val parser = breviloquence.Benchmarks.jacksonFactory.createParser(input).nn
            var count = 0L
            while parser.nextToken() != null do count += 1
            parser.close()
            count
        }
