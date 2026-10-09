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
package locomotion

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
import sedentary.*
import rudiments.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import turbulence.*
import vacuous.*

// The benchmark message schema (the `Small`/`Users`/`Logs`/… case classes) lives
// in `locomotion.BenchmarkSchema.scala`.

object Benchmarks extends Suite(m"Locomotion Protobuf codec benchmarks"):
  sealed trait Information extends Dimension
  sealed trait Bytes[Power <: Nat] extends Units[Power, Information]
  val Byte: MetricUnit[Bytes[1]] = MetricUnit(1.0)

  given byteDesignation: Designation[Bytes[1]] = () => t"B"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice
  given prefixes:        Prefixes               = Prefixes(List(Kilo, Mega, Giga, Tera))

  // ---------------------------------------------------------------------------
  // Comparison baseline: Google's protobuf-java, used through its LOW-LEVEL
  // CodedInputStream/CodedOutputStream API so no generated message classes (and
  // no `protoc` step) are required.
  //
  // Caveats — the comparison is informative but not perfectly symmetric:
  //   * Decode: this walk reads every field but builds no typed object, whereas
  //     Locomotion's typed decode also materialises case classes (Wisteria),
  //     `List`/`Map` builders and UTF-8 `Text`, so the walk does strictly less
  //     work. (Locomotion's generic `read[Protobuf]` is not benchmarked: it only
  //     wraps the payload lazily without parsing fields, so it would measure a
  //     near no-op rather than a decode.)
  //   * Encode: protobuf-java writes straight into a byte buffer, whereas
  //     Locomotion builds an intermediate `Protobuf` ADT and then prints it.
  // Read the numbers with that architectural difference in mind.
  // ---------------------------------------------------------------------------

  // Generic field walk — the analog of `read[Protobuf]`. The accumulated
  // checksum is returned so the JIT cannot dead-code-eliminate the reads. A method
  // rather than a staged body, since `TimingMain` times it too.
  def walkWithProtobufJava(bytes: scala.Array[Byte]): Long =
    walkWithProtobufJava(com.google.protobuf.CodedInputStream.newInstance(bytes).nn)

  // The same walk over a stream, for the chunked rows: protobuf-java reads it through its own
  // 4 KiB refill buffer.
  def walkWithProtobufJava(input: java.io.InputStream): Long =
    walkWithProtobufJava(com.google.protobuf.CodedInputStream.newInstance(input).nn)

  def walkWithProtobufJava(in: com.google.protobuf.CodedInputStream): Long =
    import com.google.protobuf.WireFormat
    var checksum = 0L
    var tag = in.readTag()
    while tag != 0 do
      checksum += tag
      (WireFormat.getTagWireType(tag): @unchecked) match
        case WireFormat.WIRETYPE_VARINT           => checksum += in.readRawVarint64()
        case WireFormat.WIRETYPE_FIXED64          => checksum += in.readRawLittleEndian64()
        case WireFormat.WIRETYPE_FIXED32          => checksum += in.readRawLittleEndian32()
        case WireFormat.WIRETYPE_LENGTH_DELIMITED => checksum += in.readBytes().nn.size
        case _                                    => in.skipField(tag)
      tag = in.readTag()
    checksum

  // ---------------------------------------------------------------------------
  // Corpora — in-memory values, then their encoded bytes, then plain Array[Byte]
  // views for protobuf-java. All are top-level `lazy val`s (forced once during
  // warmup) and referenced by fully-qualified name inside the staged bodies.
  // ---------------------------------------------------------------------------

  // Corpus 1: a small message with three scalar fields — exercises the tag /
  // varint / short-string fast paths.
  lazy val value1: Small = Small(42L, t"Alice", true)

  // Corpus 2: 100 user records as a repeated (unpacked) message field — the
  // typical "array of records" shape.
  lazy val value2: Users = Users:
    List.tabulate(100): index =>
      User
       ( index.toLong,
         t"user$index",
         t"user$index@example.com",
         (index & 1) == 0,
         if index%10 == 0 then t"admin" else t"user" )

  // Corpus 3: 500 log entries with six fields each — a larger throughput target
  // dominated by short strings and small integers.
  lazy val value3: Logs = Logs(List.tabulate(500)(logEntry))

  // The log-entry record shared by corpora 3 and 7 and the stress message.
  def logEntry(index: Int): LogEntry =
    val level = (index & 3) match
      case 0 => t"info"
      case 1 => t"debug"
      case 2 => t"warn"
      case _ => t"error"

    val service = (index%5) match
      case 0 => t"auth"
      case 1 => t"api"
      case 2 => t"db"
      case 3 => t"cache"
      case _ => t"worker"

    LogEntry
     ( 1700000000L + index,
       level,
       service,
       t"req-$index",
       1000L + (index%50),
       t"event $index processed" )

  // Corpus 4: 1000 integers in a packed repeated field — the varint hot path
  // with no string or message overhead.
  lazy val value4: Ints = Ints(List.tabulate(1000)(index => (index*37 + 1).toLong))

  // Corpus 5: a map with 50 string→string entries — exercises map-entry messages
  // (proto3 encodes maps as repeated key/value sub-messages).
  lazy val value5: Attributes =
    Attributes(((0 until 50).map(index => t"key$index" -> t"value$index")).to(Map))

  // Corpus 6: a message nested five levels deep — stresses nested encode/decode.
  lazy val value6: Deep1 =
    Deep1(t"level0", Deep2(t"level1", Deep3(t"level2", Deep4(t"level3", Deep5(t"level4")))))

  lazy val bytes1: Data = value1.in[Protobuf].encode
  lazy val bytes2: Data = value2.in[Protobuf].encode
  lazy val bytes3: Data = value3.in[Protobuf].encode
  lazy val bytes4: Data = value4.in[Protobuf].encode
  lazy val bytes5: Data = value5.in[Protobuf].encode
  lazy val bytes6: Data = value6.in[Protobuf].encode

  // Corpus 7: 5000 log entries (~400 KiB), large enough that a 4 KiB chunking yields ~100
  // chunks, so the chunked rows measure reading across many chunk boundaries.
  lazy val value7: Logs = Logs(List.tabulate(5000)(logEntry))

  lazy val bytes7: Data = value7.in[Protobuf].encode

  // Corpus 7 split into 4 KiB chunks, as a transport would deliver it: a strict chain of
  // distinct segments for the Locomotion rows, and the same segments as a `List` for
  // protobuf-java, which reads them through a `SequenceInputStream`.
  val ChunkSize: Int = 4096

  lazy val chunkList7: List[Data] =
    val length = bytes7.length
    List.tabulate((length + ChunkSize - 1)/ChunkSize): index =>
      val start = index*ChunkSize
      val end = (start + ChunkSize).min(length)
      bytes7.segment(start.z till end.z)

  lazy val chunks7: Chain[Data] = chunkList7.to[Chain]

  def inputStream(chunks: List[Data]): java.io.InputStream =
    val streams = new java.util.ArrayList[java.io.InputStream]()
    chunks.each: chunk =>
      streams.add(new java.io.ByteArrayInputStream(chunk.asInstanceOf[scala.Array[Byte]]))
    new java.io.SequenceInputStream(java.util.Collections.enumeration(streams))

  // The stress message: one 64 KiB block of whole `logs` occurrences re-emitted `StressBlocks`
  // times (a message's fields may be concatenated freely, so the block is simply the encoding
  // of a `Logs` with 720 entries). A `def`, not a `lazy val`, so each operation streams a
  // fresh chain, and every cell refers to the *same* block, so nothing retained by a chain
  // head grows with the message: only the parser's own buffering is measured.
  val StressBlocks: Int = 1024

  lazy val stressBlock: Data = Logs(List.tabulate(720)(logEntry)).in[Protobuf].encode

  def stressDocument: Chain[Data] =
    def repeat(n: Int): Chain[Data] =
      if n == 0 then Chain() else Chain.cons(stressBlock, repeat(n - 1))
    repeat(StressBlocks)

  lazy val stressSize: Long = StressBlocks.toLong*stressBlock.length

  // Plain Array[Byte] views for protobuf-java (the cast is sound for read-only
  // consumers; CodedInputStream never mutates its input).
  lazy val raw1: scala.Array[Byte] = bytes1.asInstanceOf[scala.Array[Byte]]
  lazy val raw2: scala.Array[Byte] = bytes2.asInstanceOf[scala.Array[Byte]]
  lazy val raw3: scala.Array[Byte] = bytes3.asInstanceOf[scala.Array[Byte]]
  lazy val raw4: scala.Array[Byte] = bytes4.asInstanceOf[scala.Array[Byte]]
  lazy val raw5: scala.Array[Byte] = bytes5.asInstanceOf[scala.Array[Byte]]
  lazy val raw6: scala.Array[Byte] = bytes6.asInstanceOf[scala.Array[Byte]]

  def run(): Unit =
    val bench = Bench()
    val constrained = Stress(heap = t"512m")

    val size1 = bytes1.length*Byte
    val size2 = bytes2.length*Byte
    val size3 = bytes3.length*Byte
    val size4 = bytes4.length*Byte
    val size5 = bytes5.length*Byte
    val size6 = bytes6.length*Byte
    val size7 = bytes7.length*Byte

    // -------------------------------------------------------------------------
    // Decode. Each corpus is decoded two ways: Locomotion typed decode (the
    // headline figure and the `Min` baseline) and the protobuf-java field walk.
    // The Locomotion row measures the full public decode path
    // (`Chain(...).read[...]`), which re-aggregates the single-chunk stream into
    // strict bytes on each iteration before parsing.
    // -------------------------------------------------------------------------

    suite(m"Decode small message (3 fields)"):
      bench(m"Decode (typed) with Locomotion")
        ( target = 1*Second, operationSize = size1 ):
        '{ Chain(locomotion.Benchmarks.bytes1).read[Small in Protobuf] }

      bench(m"Walk with protobuf-java")(target = 1*Second, operationSize = size1):
        '{ locomotion.Benchmarks.walkWithProtobufJava(locomotion.Benchmarks.raw1) }

    suite(m"Decode 100 user records"):
      bench(m"Decode (typed) with Locomotion")
        ( target = 1*Second, operationSize = size2 ):
        '{ Chain(locomotion.Benchmarks.bytes2).read[Users in Protobuf] }

      bench(m"Decode directly with Locomotion")(target = 1*Second, operationSize = size2):
        '{
            given Users is Protobuf.Parsable = locomotion.usersParsable
            Chain(locomotion.Benchmarks.bytes2).read[Users in Protobuf]
        }

      bench(m"Walk with protobuf-java")(target = 1*Second, operationSize = size2):
        '{ locomotion.Benchmarks.walkWithProtobufJava(locomotion.Benchmarks.raw2) }

    suite(m"Decode 500 log entries"):
      bench(m"Decode (typed) with Locomotion")
        ( target = 1*Second, operationSize = size3 ):
        '{ Chain(locomotion.Benchmarks.bytes3).read[Logs in Protobuf] }

      bench(m"Decode directly with Locomotion")(target = 1*Second, operationSize = size3):
        '{
            given Logs is Protobuf.Parsable = locomotion.logsParsable
            Chain(locomotion.Benchmarks.bytes3).read[Logs in Protobuf]
        }

      bench(m"Walk with protobuf-java")(target = 1*Second, operationSize = size3):
        '{ locomotion.Benchmarks.walkWithProtobufJava(locomotion.Benchmarks.raw3) }

    suite(m"Decode 1000 packed integers"):
      bench(m"Decode (typed) with Locomotion")
        ( target = 1*Second, operationSize = size4 ):
        '{ Chain(locomotion.Benchmarks.bytes4).read[Ints in Protobuf] }

      bench(m"Walk with protobuf-java")(target = 1*Second, operationSize = size4):
        '{ locomotion.Benchmarks.walkWithProtobufJava(locomotion.Benchmarks.raw4) }

    suite(m"Decode 50-entry string map"):
      bench(m"Decode (typed) with Locomotion")
        ( target = 1*Second, operationSize = size5 ):
        '{ Chain(locomotion.Benchmarks.bytes5).read[Attributes in Protobuf] }

      bench(m"Walk with protobuf-java")(target = 1*Second, operationSize = size5):
        '{ locomotion.Benchmarks.walkWithProtobufJava(locomotion.Benchmarks.raw5) }

    suite(m"Decode 5-level nested message"):
      bench(m"Decode (typed) with Locomotion")
        ( target = 1*Second, operationSize = size6 ):
        '{ Chain(locomotion.Benchmarks.bytes6).read[Deep1 in Protobuf] }

      bench(m"Decode directly with Locomotion")(target = 1*Second, operationSize = size6):
        '{
            given Deep1 is Protobuf.Parsable = locomotion.deep1Parsable
            Chain(locomotion.Benchmarks.bytes6).read[Deep1 in Protobuf]
        }

      bench(m"Walk with protobuf-java")(target = 1*Second, operationSize = size6):
        '{ locomotion.Benchmarks.walkWithProtobufJava(locomotion.Benchmarks.raw6) }

    // -------------------------------------------------------------------------
    // Chunked input: corpus 7 delivered as ~100 chunks of 4 KiB, the shape a
    // transport produces. The Locomotion rows read the public `Chain` entry
    // points (the `Protobuf` ADT path, and direct through a generated
    // `Protobuf.Parsable`); protobuf-java walks the same chunks through an
    // `InputStream`, building nothing, so it is the floor for a streaming read.
    // -------------------------------------------------------------------------

    suite(m"Decode 5000 log entries, chunked 4 KiB"):
      bench(m"Decode (typed) with Locomotion")(target = 1*Second, operationSize = size7):
        '{ locomotion.Benchmarks.chunks7.read[Logs in Protobuf] }

      bench(m"Decode directly with Locomotion")(target = 1*Second, operationSize = size7):
        '{
            given Logs is Protobuf.Parsable = locomotion.logsParsable
            locomotion.Benchmarks.chunks7.read[Logs in Protobuf]
        }

      bench(m"Count directly with Locomotion")(target = 1*Second, operationSize = size7):
        '{
            given Long is Protobuf.Parsable = locomotion.countEntries
            locomotion.Benchmarks.chunks7.read[Long in Protobuf]
        }

      bench(m"Walk with protobuf-java")(target = 1*Second, operationSize = size7):
        '{
            locomotion.Benchmarks.walkWithProtobufJava
              (locomotion.Benchmarks.inputStream(locomotion.Benchmarks.chunkList7))
        }

    // -------------------------------------------------------------------------
    // Bounded memory: a 64 MiB message streamed through the direct parser in a
    // pinned 512 MB heap. The counting consumer retains nothing, so the peak
    // heap and post-run live set are the parser's own: a parser that assembles
    // the input before decoding peaks at a multiple of the message size; a
    // streaming parser at a few chunks.
    // -------------------------------------------------------------------------

    suite(m"Stress: 64 MiB message streamed through the direct parser (512 MB heap)"):
      import parasite.threading.platformThreading

      constrained(m"Count directly with Locomotion")(target = 5*Second):
        '{
            given Long is Protobuf.Parsable = locomotion.countEntries
            locomotion.Benchmarks.stressDocument.read[Long in Protobuf]
        }

      constrained(m"Walk with protobuf-java")(target = 5*Second):
        '{
            locomotion.Benchmarks.walkWithProtobufJava
              (locomotion.Benchmarks.inputStream(locomotion.Benchmarks.stressDocument.to[List]))
        }

    // -------------------------------------------------------------------------
    // Encode. Locomotion encode is the `Min` baseline; protobuf-java rows are
    // provided for a representative subset of corpora (small scalar message,
    // repeated nested messages, packed varints), with a hand-written low-level
    // encoder: hand-writing the wire format of map entries and deep nesting adds
    // bulk without changing the picture.
    // `operationSize` is the size of the encoded output, the usual throughput
    // denominator for serialisation.
    // -------------------------------------------------------------------------

    suite(m"Encode small message (3 fields)"):
      bench(m"Encode with Locomotion")
        ( target = 1*Second, operationSize = size1 ):
        '{ locomotion.Benchmarks.value1.in[Protobuf].encode }

      bench(m"Encode with protobuf-java")(target = 1*Second, operationSize = size1):
        '{
            val out = new _root_.java.io.ByteArrayOutputStream(32)
            val cos = com.google.protobuf.CodedOutputStream.newInstance(out).nn
            cos.writeInt64(1, 42L)
            cos.writeString(2, "Alice")
            cos.writeBool(3, true)
            cos.flush()
            out.toByteArray.nn
        }

    suite(m"Encode 100 user records"):
      bench(m"Encode with Locomotion")
        ( target = 1*Second, operationSize = size2 ):
        '{ locomotion.Benchmarks.value2.in[Protobuf].encode }

      bench(m"Encode with protobuf-java")(target = 1*Second, operationSize = size2):
        '{
            val out = new _root_.java.io.ByteArrayOutputStream(8192)
            val cos = com.google.protobuf.CodedOutputStream.newInstance(out).nn
            var index = 0

            while index < 100 do
              val sub = new _root_.java.io.ByteArrayOutputStream(64)
              val scos = com.google.protobuf.CodedOutputStream.newInstance(sub).nn
              scos.writeInt64(1, index.toLong)
              scos.writeString(2, s"user$index")
              scos.writeString(3, s"user$index@example.com")
              scos.writeBool(4, (index & 1) == 0)
              scos.writeString(5, if index%10 == 0 then "admin" else "user")
              scos.flush()
              val message = sub.toByteArray.nn
              cos.writeTag(1, com.google.protobuf.WireFormat.WIRETYPE_LENGTH_DELIMITED)
              cos.writeUInt32NoTag(message.length)
              cos.writeRawBytes(message)
              index += 1

            cos.flush()
            out.toByteArray.nn
        }

    suite(m"Encode 1000 packed integers"):
      bench(m"Encode with Locomotion")
        ( target = 1*Second, operationSize = size4 ):
        '{ locomotion.Benchmarks.value4.in[Protobuf].encode }

      bench(m"Encode with protobuf-java")(target = 1*Second, operationSize = size4):
        '{
            val out = new _root_.java.io.ByteArrayOutputStream(4096)
            val cos = com.google.protobuf.CodedOutputStream.newInstance(out).nn
            val body = new _root_.java.io.ByteArrayOutputStream(4096)
            val bcos = com.google.protobuf.CodedOutputStream.newInstance(body).nn
            var index = 0
            while index < 1000 do { bcos.writeInt64NoTag((index*37 + 1).toLong); index += 1 }
            bcos.flush()
            val packed = body.toByteArray.nn
            cos.writeTag(1, com.google.protobuf.WireFormat.WIRETYPE_LENGTH_DELIMITED)
            cos.writeUInt32NoTag(packed.length)
            cos.writeRawBytes(packed)
            cos.flush()
            out.toByteArray.nn
        }

    suite(m"Encode 500 log entries (Locomotion only)"):
      bench(m"Encode with Locomotion")
        ( target = 1*Second, operationSize = size3 ):
        '{ locomotion.Benchmarks.value3.in[Protobuf].encode }

    suite(m"Encode 50-entry string map (Locomotion only)"):
      bench(m"Encode with Locomotion")
        ( target = 1*Second, operationSize = size5 ):
        '{ locomotion.Benchmarks.value5.in[Protobuf].encode }

    suite(m"Encode 5-level nested message (Locomotion only)"):
      bench(m"Encode with Locomotion")
        ( target = 1*Second, operationSize = size6 ):
        '{ locomotion.Benchmarks.value6.in[Protobuf].encode }
