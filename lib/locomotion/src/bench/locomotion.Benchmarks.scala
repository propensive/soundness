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
package locomotion

import ambience.*, environments.javaEnvironment, systems.javaSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
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
  // checksum is returned so the JIT cannot dead-code-eliminate the reads.
  def walkWithProtobufJava(bytes: Array[Byte]): Long =
    import com.google.protobuf.WireFormat
    val in = com.google.protobuf.CodedInputStream.newInstance(bytes).nn
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

  // protobuf-java encoders for a representative subset of corpora (small scalar
  // message, repeated nested messages, packed varints). The remaining corpora
  // (map entries, deep nesting) are omitted on the protobuf-java side because
  // hand-writing their wire format adds bulk without changing the picture.
  def encodeSmallWithProtobufJava: Array[Byte] =
    val out = new _root_.java.io.ByteArrayOutputStream(32)
    val cos = com.google.protobuf.CodedOutputStream.newInstance(out).nn
    cos.writeInt64(1, 42L)
    cos.writeString(2, "Alice")
    cos.writeBool(3, true)
    cos.flush()
    out.toByteArray.nn

  def encodeUsersWithProtobufJava: Array[Byte] =
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

  def encodeIntsWithProtobufJava: Array[Byte] =
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
  lazy val value3: Logs =
    val levels   = IArray(t"info", t"debug", t"warn", t"error")
    val services = IArray(t"auth", t"api", t"db", t"cache", t"worker")
    Logs:
      List.tabulate(500): index =>
        LogEntry
         ( 1700000000L + index,
           levels(index & 3),
           services(index%5),
           t"req-$index",
           1000L + (index%50),
           t"event $index processed" )

  // Corpus 4: 1000 integers in a packed repeated field — the varint hot path
  // with no string or message overhead.
  lazy val value4: Ints = Ints(List.tabulate(1000)(index => (index*37 + 1).toLong))

  // Corpus 5: a map with 50 string→string entries — exercises map-entry messages
  // (proto3 encodes maps as repeated key/value sub-messages).
  lazy val value5: Attributes =
    Attributes((0 until 50).map(index => t"key$index" -> t"value$index").to(Map))

  // Corpus 6: a message nested five levels deep — stresses nested encode/decode.
  lazy val value6: Deep1 =
    Deep1(t"level0", Deep2(t"level1", Deep3(t"level2", Deep4(t"level3", Deep5(t"level4")))))

  lazy val bytes1: Data = value1.protobuf.encode
  lazy val bytes2: Data = value2.protobuf.encode
  lazy val bytes3: Data = value3.protobuf.encode
  lazy val bytes4: Data = value4.protobuf.encode
  lazy val bytes5: Data = value5.protobuf.encode
  lazy val bytes6: Data = value6.protobuf.encode

  // Plain Array[Byte] views for protobuf-java (the cast is sound for read-only
  // consumers; CodedInputStream never mutates its input).
  lazy val raw1: Array[Byte] = bytes1.asInstanceOf[Array[Byte]]
  lazy val raw2: Array[Byte] = bytes2.asInstanceOf[Array[Byte]]
  lazy val raw3: Array[Byte] = bytes3.asInstanceOf[Array[Byte]]
  lazy val raw4: Array[Byte] = bytes4.asInstanceOf[Array[Byte]]
  lazy val raw5: Array[Byte] = bytes5.asInstanceOf[Array[Byte]]
  lazy val raw6: Array[Byte] = bytes6.asInstanceOf[Array[Byte]]

  // The benchmark bodies are staged and recompiled by superlunary, so the
  // contextual `Tactic[ProtobufError]` and the derived codec instances must be
  // resolved here (where the imports are in scope) rather than inside the quoted
  // body. Each operation therefore goes through a fully-qualified helper method
  // the quote simply invokes.

  def decodeSmall:      Small      = Stream(bytes1).read[Small in Protobuf]
  def decodeUsers:      Users      = Stream(bytes2).read[Users in Protobuf]
  def decodeLogs:       Logs       = Stream(bytes3).read[Logs in Protobuf]
  def decodeInts:       Ints       = Stream(bytes4).read[Ints in Protobuf]
  def decodeAttributes: Attributes = Stream(bytes5).read[Attributes in Protobuf]
  def decodeNested:     Deep1      = Stream(bytes6).read[Deep1 in Protobuf]

  def encodeSmall:      Data = value1.protobuf.encode
  def encodeUsers:      Data = value2.protobuf.encode
  def encodeLogs:       Data = value3.protobuf.encode
  def encodeInts:       Data = value4.protobuf.encode
  def encodeAttributes: Data = value5.protobuf.encode
  def encodeNested:     Data = value6.protobuf.encode

  def run(): Unit =
    val bench = Bench()

    val size1 = bytes1.length*Byte
    val size2 = bytes2.length*Byte
    val size3 = bytes3.length*Byte
    val size4 = bytes4.length*Byte
    val size5 = bytes5.length*Byte
    val size6 = bytes6.length*Byte

    // -------------------------------------------------------------------------
    // Decode. Each corpus is decoded two ways: Locomotion typed decode (the
    // headline figure and the `Min` baseline) and the protobuf-java field walk.
    // The Locomotion row measures the full public decode path
    // (`Stream(...).read[...]`), which re-aggregates the single-chunk stream into
    // strict bytes on each iteration before parsing.
    // -------------------------------------------------------------------------

    suite(m"Decode small message (3 fields)"):
      bench(m"Decode (typed) with Locomotion")
        ( target = 1*Second, operationSize = size1, baseline = Baseline(compare = Min) ):
        '{ locomotion.Benchmarks.decodeSmall }

      bench(m"Walk with protobuf-java")(target = 1*Second, operationSize = size1):
        '{ locomotion.Benchmarks.walkWithProtobufJava(locomotion.Benchmarks.raw1) }

    suite(m"Decode 100 user records"):
      bench(m"Decode (typed) with Locomotion")
        ( target = 1*Second, operationSize = size2, baseline = Baseline(compare = Min) ):
        '{ locomotion.Benchmarks.decodeUsers }

      bench(m"Walk with protobuf-java")(target = 1*Second, operationSize = size2):
        '{ locomotion.Benchmarks.walkWithProtobufJava(locomotion.Benchmarks.raw2) }

    suite(m"Decode 500 log entries"):
      bench(m"Decode (typed) with Locomotion")
        ( target = 1*Second, operationSize = size3, baseline = Baseline(compare = Min) ):
        '{ locomotion.Benchmarks.decodeLogs }

      bench(m"Walk with protobuf-java")(target = 1*Second, operationSize = size3):
        '{ locomotion.Benchmarks.walkWithProtobufJava(locomotion.Benchmarks.raw3) }

    suite(m"Decode 1000 packed integers"):
      bench(m"Decode (typed) with Locomotion")
        ( target = 1*Second, operationSize = size4, baseline = Baseline(compare = Min) ):
        '{ locomotion.Benchmarks.decodeInts }

      bench(m"Walk with protobuf-java")(target = 1*Second, operationSize = size4):
        '{ locomotion.Benchmarks.walkWithProtobufJava(locomotion.Benchmarks.raw4) }

    suite(m"Decode 50-entry string map"):
      bench(m"Decode (typed) with Locomotion")
        ( target = 1*Second, operationSize = size5, baseline = Baseline(compare = Min) ):
        '{ locomotion.Benchmarks.decodeAttributes }

      bench(m"Walk with protobuf-java")(target = 1*Second, operationSize = size5):
        '{ locomotion.Benchmarks.walkWithProtobufJava(locomotion.Benchmarks.raw5) }

    suite(m"Decode 5-level nested message"):
      bench(m"Decode (typed) with Locomotion")
        ( target = 1*Second, operationSize = size6, baseline = Baseline(compare = Min) ):
        '{ locomotion.Benchmarks.decodeNested }

      bench(m"Walk with protobuf-java")(target = 1*Second, operationSize = size6):
        '{ locomotion.Benchmarks.walkWithProtobufJava(locomotion.Benchmarks.raw6) }

    // -------------------------------------------------------------------------
    // Encode. Locomotion encode is the `Min` baseline; protobuf-java rows are
    // provided for the corpora with a hand-written low-level encoder.
    // `operationSize` is the size of the encoded output, the usual throughput
    // denominator for serialisation.
    // -------------------------------------------------------------------------

    suite(m"Encode small message (3 fields)"):
      bench(m"Encode with Locomotion")
        ( target = 1*Second, operationSize = size1, baseline = Baseline(compare = Min) ):
        '{ locomotion.Benchmarks.encodeSmall }

      bench(m"Encode with protobuf-java")(target = 1*Second, operationSize = size1):
        '{ locomotion.Benchmarks.encodeSmallWithProtobufJava }

    suite(m"Encode 100 user records"):
      bench(m"Encode with Locomotion")
        ( target = 1*Second, operationSize = size2, baseline = Baseline(compare = Min) ):
        '{ locomotion.Benchmarks.encodeUsers }

      bench(m"Encode with protobuf-java")(target = 1*Second, operationSize = size2):
        '{ locomotion.Benchmarks.encodeUsersWithProtobufJava }

    suite(m"Encode 1000 packed integers"):
      bench(m"Encode with Locomotion")
        ( target = 1*Second, operationSize = size4, baseline = Baseline(compare = Min) ):
        '{ locomotion.Benchmarks.encodeInts }

      bench(m"Encode with protobuf-java")(target = 1*Second, operationSize = size4):
        '{ locomotion.Benchmarks.encodeIntsWithProtobufJava }

    suite(m"Encode 500 log entries (Locomotion only)"):
      bench(m"Encode with Locomotion")
        ( target = 1*Second, operationSize = size3, baseline = Baseline(compare = Min) ):
        '{ locomotion.Benchmarks.encodeLogs }

    suite(m"Encode 50-entry string map (Locomotion only)"):
      bench(m"Encode with Locomotion")
        ( target = 1*Second, operationSize = size5, baseline = Baseline(compare = Min) ):
        '{ locomotion.Benchmarks.encodeAttributes }

    suite(m"Encode 5-level nested message (Locomotion only)"):
      bench(m"Encode with Locomotion")
        ( target = 1*Second, operationSize = size6, baseline = Baseline(compare = Min) ):
        '{ locomotion.Benchmarks.encodeNested }
