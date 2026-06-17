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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import ambience.*, environments.javaEnvironment, systems.javaSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import probably.*
import proscenium.*
import quantitative.*
import sedentary.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
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

  def parseWithJackson(bytes: Array[Byte]): com.fasterxml.jackson.databind.JsonNode =
    jacksonMapper.readTree(bytes).nn

  def parseWithBorer(bytes: Array[Byte]): io.bullet.borer.Dom.Element =
    io.bullet.borer.Cbor.decode(bytes).to[io.bullet.borer.Dom.Element].value

  // Helper to build CBOR bytes by hand for the benchmark corpora. Uses the
  // canonical encoder so the inputs are well-formed and deterministic.
  def encode(ast: Cbor.Ast): IArray[Byte] = Cbor.Ast.encodable.encoded(ast)

  // Corpus 1: a small object with three string-keyed entries (id, name, active).
  // Roughly 30 bytes — exercises the head-byte fast path for short strings and
  // a small definite-length map.
  lazy val cborBytes1: IArray[Byte] =
    val keys = IArray[Any]("id", "name", "active")
    val values = IArray[Any](42L, "Alice", true)
    encode(Cbor.Ast.map(keys, values))

  // Corpus 2: 100 user records — typical "array of records" pattern with five
  // repeated keys per element.
  lazy val cborBytes2: IArray[Byte] =
    val records = (0 until 100).map: index =>
      val keys = IArray[Any]("id", "username", "email", "active", "role")
      val active = (index&1) == 0
      val role = if index%10 == 0 then "admin" else "user"
      val values = IArray[Any](index.toLong, s"user$index", s"user$index@example.com", active, role)
      Cbor.Ast.map(keys, values).asInstanceOf[Any]
    encode(Cbor.Ast.map(IArray[Any]("users"), IArray[Any](Cbor.Ast.array(IArray.from(records)))))

  // Corpus 3: 500 log entries with six keys each — larger throughput target,
  // dominated by short-string parsing and small-integer head bytes.
  lazy val cborBytes3: IArray[Byte] =
    val levels = Array("info", "debug", "warn", "error")
    val services = Array("auth", "api", "db", "cache", "worker")
    val records = (0 until 500).map: index =>
      val keys = IArray[Any]("timestamp", "level", "service", "requestId", "userId", "message")
      val ts = 1700000000L + index
      val level = levels(index & 3)
      val service = services(index % 5)
      val userId = 1000L + (index % 50)
      val values = IArray[Any](ts, level, service, s"req-$index", userId, s"event $index processed")
      Cbor.Ast.map(keys, values).asInstanceOf[Any]
    encode(Cbor.Ast.map(IArray[Any]("logs"), IArray[Any](Cbor.Ast.array(IArray.from(records)))))

  // Corpus 4: 1000 small integers — exercises the integer head-byte hot path
  // without string or map overhead.
  lazy val cborBytes4: IArray[Byte] =
    val items = IArray.from((0 until 1000).map{ index => (index*37 + 1).toLong.asInstanceOf[Any] })
    encode(Cbor.Ast.array(items))

  // Corpus 5: 100 byte-string records — exercises major-type-2 (byte strings),
  // which JSON has no analog for.
  lazy val cborBytes5: IArray[Byte] =
    val records = (0 until 100).map: index =>
      val payload = new Array[Byte](32)
      var j = 0
      while j < payload.length do { payload(j) = ((index + j) & 0xFF).toByte; j += 1 }
      payload.asInstanceOf[IArray[Byte]].asInstanceOf[Any]

    encode(Cbor.Ast.array(IArray.from(records)))

  // Corpus 6: deeply nested structure (10-level wrapping) — stresses recursion
  // and the small-array head-byte path.
  lazy val cborBytes6: IArray[Byte] =
    var ast: Any = "deep"
    var index = 0
    while index < 10 do
      ast = Cbor.Ast.map(IArray[Any](s"level$index"), IArray[Any](ast))
      index += 1
    encode(ast.asInstanceOf[Cbor.Ast])

  // Pre-converted to plain Array[Byte] for the comparison parsers (Jackson and
  // borer take Array[Byte]; the IArray.unsafeMutable cast is safe for read-only
  // benchmarks since neither parser mutates the input).
  lazy val raw1: Array[Byte] = cborBytes1.asInstanceOf[Array[Byte]]
  lazy val raw2: Array[Byte] = cborBytes2.asInstanceOf[Array[Byte]]
  lazy val raw3: Array[Byte] = cborBytes3.asInstanceOf[Array[Byte]]
  lazy val raw4: Array[Byte] = cborBytes4.asInstanceOf[Array[Byte]]
  lazy val raw5: Array[Byte] = cborBytes5.asInstanceOf[Array[Byte]]
  lazy val raw6: Array[Byte] = cborBytes6.asInstanceOf[Array[Byte]]

  def run(): Unit =
    val bench = Bench()

    val size1 = cborBytes1.length*Byte
    val size2 = cborBytes2.length*Byte
    val size3 = cborBytes3.length*Byte
    val size4 = cborBytes4.length*Byte
    val size5 = cborBytes5.length*Byte
    val size6 = cborBytes6.length*Byte

    suite(m"Parse small object (3 fields)"):
      bench(m"Parse with Breviloquence")
        ( target = 1*Second, operationSize = size1, baseline = Baseline(compare = Min) ):
        '{ Cbor.Ast.parse(breviloquence.Benchmarks.cborBytes1) }

      bench(m"Parse with Jackson")(target = 1*Second, operationSize = size1):
        '{ breviloquence.Benchmarks.parseWithJackson(breviloquence.Benchmarks.raw1) }

      bench(m"Parse with borer")(target = 1*Second, operationSize = size1):
        '{ breviloquence.Benchmarks.parseWithBorer(breviloquence.Benchmarks.raw1) }

    suite(m"Parse 100 user records"):
      bench(m"Parse with Breviloquence")
        ( target = 1*Second, operationSize = size2, baseline = Baseline(compare = Min) ):
        '{ Cbor.Ast.parse(breviloquence.Benchmarks.cborBytes2) }

      bench(m"Parse with Jackson")(target = 1*Second, operationSize = size2):
        '{ breviloquence.Benchmarks.parseWithJackson(breviloquence.Benchmarks.raw2) }

      bench(m"Parse with borer")(target = 1*Second, operationSize = size2):
        '{ breviloquence.Benchmarks.parseWithBorer(breviloquence.Benchmarks.raw2) }

    suite(m"Parse 500 log entries"):
      bench(m"Parse with Breviloquence")
        ( target = 1*Second, operationSize = size3, baseline = Baseline(compare = Min) ):
        '{ Cbor.Ast.parse(breviloquence.Benchmarks.cborBytes3) }

      bench(m"Parse with Jackson")(target = 1*Second, operationSize = size3):
        '{ breviloquence.Benchmarks.parseWithJackson(breviloquence.Benchmarks.raw3) }

      bench(m"Parse with borer")(target = 1*Second, operationSize = size3):
        '{ breviloquence.Benchmarks.parseWithBorer(breviloquence.Benchmarks.raw3) }

    suite(m"Parse 1000 small integers"):
      bench(m"Parse with Breviloquence")
        ( target = 1*Second, operationSize = size4, baseline = Baseline(compare = Min) ):
        '{ Cbor.Ast.parse(breviloquence.Benchmarks.cborBytes4) }

      bench(m"Parse with Jackson")(target = 1*Second, operationSize = size4):
        '{ breviloquence.Benchmarks.parseWithJackson(breviloquence.Benchmarks.raw4) }

      bench(m"Parse with borer")(target = 1*Second, operationSize = size4):
        '{ breviloquence.Benchmarks.parseWithBorer(breviloquence.Benchmarks.raw4) }

    suite(m"Parse 100 byte strings"):
      bench(m"Parse with Breviloquence")
        ( target = 1*Second, operationSize = size5, baseline = Baseline(compare = Min) ):
        '{ Cbor.Ast.parse(breviloquence.Benchmarks.cborBytes5) }

      bench(m"Parse with Jackson")(target = 1*Second, operationSize = size5):
        '{ breviloquence.Benchmarks.parseWithJackson(breviloquence.Benchmarks.raw5) }

      bench(m"Parse with borer")(target = 1*Second, operationSize = size5):
        '{ breviloquence.Benchmarks.parseWithBorer(breviloquence.Benchmarks.raw5) }

    suite(m"Parse 10-level nested map"):
      bench(m"Parse with Breviloquence")
        ( target = 1*Second, operationSize = size6, baseline = Baseline(compare = Min) ):
        '{ Cbor.Ast.parse(breviloquence.Benchmarks.cborBytes6) }

      bench(m"Parse with Jackson")(target = 1*Second, operationSize = size6):
        '{ breviloquence.Benchmarks.parseWithJackson(breviloquence.Benchmarks.raw6) }

      bench(m"Parse with borer")(target = 1*Second, operationSize = size6):
        '{ breviloquence.Benchmarks.parseWithBorer(breviloquence.Benchmarks.raw6) }
