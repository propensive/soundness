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
package jacinta

import scala.quoted.*

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
import rudiments.*
import sedentary.*
import spectacular.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import turbulence.*
import vacuous.*

// Typed targets for the decode benchmarks (Example 4's shape). `BenchUsers`
// opts in to direct parsing, so `read[BenchUsers in Json]` never builds the
// AST that the `read[Json].as[BenchUsers]` arm materializes.
case class BenchUser(id: Int, username: Text, email: Text, active: Boolean, role: Text)

object BenchUser:
  given parsable: BenchUser is Json.Parsable = Json.Parsable.derived

case class BenchUsers(users: List[BenchUser])

object BenchUsers:
  given parsable: BenchUsers is Json.Parsable = Json.Parsable.derived

// Jsoniter's view of the same records, with `String` fields as a Jsoniter
// user would declare them (`Text` is an opaque zero-cost wrapper over
// `String`, so the two targets are materially identical).
case class JsoniterUser(id: Int, username: String, email: String, active: Boolean, role: String)
case class JsoniterUsers(users: List[JsoniterUser])

object Benchmarks extends Suite(m"Jacinta JSON parser benchmarks"):
  sealed trait Information extends Dimension
  sealed trait Bytes[Power <: Nat] extends Units[Power, Information]
  val Byte: MetricUnit[Bytes[1]] = MetricUnit(1.0)

  given byteDesignation: Designation[Bytes[1]] = () => t"B"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice

  // Auto-scale byte sizes (B → kB → MB → GB → TB) and byte rates so the
  // table prints "1.3 GB·s¯¹" instead of "1.3×10⁹ B·s¯¹".
  given prefixes: Prefixes = Prefixes(List(Kilo, Mega, Giga, Tera))

  // Codec used by the Jsoniter benchmark to parse into a circe `Json` AST.
  val jsoniterCodec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[io.circe.Json] =
    com.github.plokhotnyuk.jsoniter_scala.circe.JsoniterScalaCodec.jsonCodec()

  def parseWithJsoniter(text: String): io.circe.Json =
    com.github.plokhotnyuk.jsoniter_scala.core.readFromString[io.circe.Json](text)
      ( using jsoniterCodec )

  // Jackson tree-model parser (closest analog to the other parsers' AST
  // outputs). The `ObjectMapper` is shared across iterations because
  // construction is expensive and is intended to be amortised in production.
  val jacksonMapper: com.fasterxml.jackson.databind.ObjectMapper =
    new com.fasterxml.jackson.databind.ObjectMapper()

  def parseWithJackson(text: String): com.fasterxml.jackson.databind.JsonNode =
    jacksonMapper.readTree(text).nn

  // The decode arms: materialize the AST and walk it with `Decodable`;
  // parse tokens straight into the records with `Parsable`; or use
  // Jsoniter's macro-generated direct codec (the state of the art for
  // direct-to-case-class parsing on the JVM).
  def decodeUsersAst(): BenchUsers = LazyList(jsonBytes4).read[Json].as[BenchUsers]
  def decodeUsersDirect(): BenchUsers = LazyList(jsonBytes4).read[BenchUsers in Json]
  def decodeUsersDirectData(): BenchUsers = jsonBytes4.read[BenchUsers in Json]

  // A hand-written parser over the public reader API: the "ceiling" for
  // monomorphic user code, isolating the derivation interpreter's overhead
  // from the tokenizer's.
  val handUsersParsable: BenchUsers is Json.Parsable =
    Json.Parsable(Morphology.Any): reader =>
      def user(): BenchUser =
        reader.openObject()
        var id = 0
        var username: Text = t""
        var email: Text = t""
        var active = false
        var role: Text = t""

        while
          reader.key().lay(false): key =>
            if key == t"id" then id = reader.int()
            else if key == t"username" then username = reader.string()
            else if key == t"email" then email = reader.string()
            else if key == t"active" then active = reader.boolean()
            else if key == t"role" then role = reader.string()
            else reader.skipValue()
            true
        do ()

        BenchUser(id, username, email, active, role)

      reader.openObject()
      var users: List[BenchUser] = Nil

      while
        reader.key().lay(false): key =>
          if key == t"users" then
            val builder = List.newBuilder[BenchUser]
            reader.openArray()
            while reader.element() do builder += user()
            users = builder.result()
          else reader.skipValue()
          true
      do ()

      BenchUsers(users)

  // Scans and validates every token of the document but materializes
  // nothing: the tokenizer's floor, separating scan cost from the cost of
  // building strings and records.
  val skimParsable: Unit is Json.Parsable =
    Json.Parsable(Morphology.Any)(_.skipValue())

  def skimUsers(): Unit =
    given Unit is Json.Parsable = skimParsable
    jsonBytes4.read[Unit in Json]

  def decodeUsersHand(): BenchUsers =
    given BenchUsers is Json.Parsable = handUsersParsable
    jsonBytes4.read[BenchUsers in Json]

  val jsoniterUsersCodec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[JsoniterUsers] =
    com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker.make

  def decodeUsersJsoniter(): JsoniterUsers =
    com.github.plokhotnyuk.jsoniter_scala.core.readFromArray[JsoniterUsers]
      ( jsonBytes4.mutable(using Unsafe) )(using jsoniterUsersCodec)

  def run(): Unit =
    val bench = Bench()

    val size1 = jsonBytes1.length*Byte
    val size2 = jsonBytes2.length*Byte
    val size3 = jsonBytes3.length*Byte
    val size4 = jsonBytes4.length*Byte
    val size5 = jsonBytes5.length*Byte
    val size6 = jsonBytes6.length*Byte

    suite(m"Parse example 1"):
      bench(m"Parse file with Merino")
        ( target = 1*Second, operationSize = size1, baseline = Baseline(compare = Min) ):
        '{ Json.Ast.parse(jacinta.Benchmarks.jsonBytes1) }

      bench(m"Parse file with Merino (tracking)")(target = 1*Second, operationSize = size1):
        '{ Json.Ast.parseTracked(jacinta.Benchmarks.jsonBytes1) }

      bench(m"Parse file with Jawn")(target = 1*Second, operationSize = size1):
        ' {
            import org.typelevel.jawn.ast.JParser
            JParser.parseFromString(jacinta.Benchmarks.jsonText1)
          }

      bench(m"Parse file with Circe")(target = 1*Second, operationSize = size1):
        '{ io.circe.parser.parse(jacinta.Benchmarks.jsonText1) }

      bench(m"Parse file with Jsoniter")(target = 1*Second, operationSize = size1):
        '{ jacinta.Benchmarks.parseWithJsoniter(jacinta.Benchmarks.jsonText1) }

      bench(m"Parse file with Jackson")(target = 1*Second, operationSize = size1):
        '{ jacinta.Benchmarks.parseWithJackson(jacinta.Benchmarks.jsonText1) }

    suite(m"Parse example 2"):
      bench(m"Parse file with Merino")
        ( target = 1*Second, operationSize = size2, baseline = Baseline(compare = Min) ):
        '{ Json.Ast.parse(jacinta.Benchmarks.jsonBytes2) }

      bench(m"Parse file with Merino (tracking)")(target = 1*Second, operationSize = size2):
        '{ Json.Ast.parseTracked(jacinta.Benchmarks.jsonBytes2) }

      bench(m"Parse file with Jawn")(target = 1*Second, operationSize = size2):
        ' {
            import org.typelevel.jawn.ast.JParser
            JParser.parseFromString(jacinta.Benchmarks.jsonText2)
          }

      bench(m"Parse file with Circe")(target = 1*Second, operationSize = size2):
        '{ io.circe.parser.parse(jacinta.Benchmarks.jsonText2) }

      bench(m"Parse file with Jsoniter")(target = 1*Second, operationSize = size2):
        '{ jacinta.Benchmarks.parseWithJsoniter(jacinta.Benchmarks.jsonText2) }

      bench(m"Parse file with Jackson")(target = 1*Second, operationSize = size2):
        '{ jacinta.Benchmarks.parseWithJackson(jacinta.Benchmarks.jsonText2) }

    suite(m"Parse example 3"):
      bench(m"Parse file with Merino")
        ( target = 1*Second, operationSize = size3, baseline = Baseline(compare = Min) ):
        '{ Json.Ast.parse(jacinta.Benchmarks.jsonBytes3) }

      bench(m"Parse file with Merino (tracking)")(target = 1*Second, operationSize = size3):
        '{ Json.Ast.parseTracked(jacinta.Benchmarks.jsonBytes3) }

      bench(m"Parse file with Jawn")(target = 1*Second, operationSize = size3):
        ' {
            import org.typelevel.jawn.ast.JParser
            JParser.parseFromString(jacinta.Benchmarks.jsonText3)
          }

      bench(m"Parse file with Circe")(target = 1*Second, operationSize = size3):
        '{ io.circe.parser.parse(jacinta.Benchmarks.jsonText3) }

      bench(m"Parse file with Jsoniter")(target = 1*Second, operationSize = size3):
        '{ jacinta.Benchmarks.parseWithJsoniter(jacinta.Benchmarks.jsonText3) }

      bench(m"Parse file with Jackson")(target = 1*Second, operationSize = size3):
        '{ jacinta.Benchmarks.parseWithJackson(jacinta.Benchmarks.jsonText3) }

    suite(m"Parse example 4 (100 user records)"):
      bench(m"Parse file with Merino")
        ( target = 1*Second, operationSize = size4, baseline = Baseline(compare = Min) ):
        '{ Json.Ast.parse(jacinta.Benchmarks.jsonBytes4) }

      bench(m"Parse file with Merino (tracking)")(target = 1*Second, operationSize = size4):
        '{ Json.Ast.parseTracked(jacinta.Benchmarks.jsonBytes4) }

      bench(m"Parse file with Jawn")(target = 1*Second, operationSize = size4):
        ' {
            import org.typelevel.jawn.ast.JParser
            JParser.parseFromString(jacinta.Benchmarks.jsonText4)
          }

      bench(m"Parse file with Circe")(target = 1*Second, operationSize = size4):
        '{ io.circe.parser.parse(jacinta.Benchmarks.jsonText4) }

      bench(m"Parse file with Jsoniter")(target = 1*Second, operationSize = size4):
        '{ jacinta.Benchmarks.parseWithJsoniter(jacinta.Benchmarks.jsonText4) }

      bench(m"Parse file with Jackson")(target = 1*Second, operationSize = size4):
        '{ jacinta.Benchmarks.parseWithJackson(jacinta.Benchmarks.jsonText4) }

    suite(m"Decode example 4 into records (100 user records)"):
      bench(m"Decode via the Json AST")
        ( target = 1*Second, operationSize = size4, baseline = Baseline(compare = Min) ):
        '{ jacinta.Benchmarks.decodeUsersAst() }

      bench(m"Decode directly with Parsable")(target = 1*Second, operationSize = size4):
        '{ jacinta.Benchmarks.decodeUsersDirect() }

      bench(m"Decode directly with Parsable (whole Data)")
        ( target = 1*Second, operationSize = size4 ):
        '{ jacinta.Benchmarks.decodeUsersDirectData() }

      bench(m"Scan all tokens, materialize nothing")(target = 1*Second, operationSize = size4):
        '{ jacinta.Benchmarks.skimUsers() }

      bench(m"Decode with a hand-written Parsable")(target = 1*Second, operationSize = size4):
        '{ jacinta.Benchmarks.decodeUsersHand() }

      bench(m"Decode directly with Jsoniter")(target = 1*Second, operationSize = size4):
        '{ jacinta.Benchmarks.decodeUsersJsoniter() }

    suite(m"Parse example 5 (500 log entries)"):
      bench(m"Parse file with Merino")
        ( target = 1*Second, operationSize = size5, baseline = Baseline(compare = Min) ):
        '{ Json.Ast.parse(jacinta.Benchmarks.jsonBytes5) }

      bench(m"Parse file with Merino (tracking)")(target = 1*Second, operationSize = size5):
        '{ Json.Ast.parseTracked(jacinta.Benchmarks.jsonBytes5) }

      bench(m"Parse file with Jawn")(target = 1*Second, operationSize = size5):
        ' {
            import org.typelevel.jawn.ast.JParser
            JParser.parseFromString(jacinta.Benchmarks.jsonText5)
          }

      bench(m"Parse file with Circe")(target = 1*Second, operationSize = size5):
        '{ io.circe.parser.parse(jacinta.Benchmarks.jsonText5) }

      bench(m"Parse file with Jsoniter")(target = 1*Second, operationSize = size5):
        '{ jacinta.Benchmarks.parseWithJsoniter(jacinta.Benchmarks.jsonText5) }

      bench(m"Parse file with Jackson")(target = 1*Second, operationSize = size5):
        '{ jacinta.Benchmarks.parseWithJackson(jacinta.Benchmarks.jsonText5) }

    suite(m"Parse example 6 (50 high-precision blockchain transactions)"):
      // Three Merino rows exercising each `NumberMode` to visualise the
      // precision-vs-throughput trade-off on inputs that overflow the
      // in-Long fast path (every wei/gas value here is >15 nibbles).
      bench(m"Parse file with Merino (Full / Bcd Array[Double])")
        ( target = 1*Second, operationSize = size6, baseline = Baseline(compare = Min) ):
        '{ Json.Ast.parse(jacinta.Benchmarks.jsonBytes6)(using jacinta.NumberMode.Full) }

      bench(m"Parse file with Merino (Full, tracking)")
        (target = 1*Second, operationSize = size6):
        '{ Json.Ast.parseTracked(jacinta.Benchmarks.jsonBytes6)(using jacinta.NumberMode.Full) }

      bench(m"Parse file with Merino (Bcd single Long)")
        (target = 1*Second, operationSize = size6):
        '{ Json.Ast.parse(jacinta.Benchmarks.jsonBytes6)(using jacinta.NumberMode.Bcd) }

      bench(m"Parse file with Merino (Double)")
        (target = 1*Second, operationSize = size6):
        '{ Json.Ast.parse(jacinta.Benchmarks.jsonBytes6)(using jacinta.NumberMode.Double) }

      bench(m"Parse file with Jawn")(target = 1*Second, operationSize = size6):
        ' {
            import org.typelevel.jawn.ast.JParser
            JParser.parseFromString(jacinta.Benchmarks.jsonText6)
          }

      bench(m"Parse file with Circe")(target = 1*Second, operationSize = size6):
        '{ io.circe.parser.parse(jacinta.Benchmarks.jsonText6) }

      bench(m"Parse file with Jsoniter")(target = 1*Second, operationSize = size6):
        '{ jacinta.Benchmarks.parseWithJsoniter(jacinta.Benchmarks.jsonText6) }

      bench(m"Parse file with Jackson")(target = 1*Second, operationSize = size6):
        '{ jacinta.Benchmarks.parseWithJackson(jacinta.Benchmarks.jsonText6) }

    suite(m"Print high-precision-number AST"):
      bench(m"Print blockchain example (50 transactions)")(target = 1*Second):
        '{ jacinta.Benchmarks.printBlockchain() }

    suite(m"Parse example 7 (1000 small integers)"):
      val size7 = jsonBytes7.length*Byte

      bench(m"Parse file with Merino")
        ( target = 1*Second, operationSize = size7, baseline = Baseline(compare = Min) ):
        '{ Json.Ast.parse(jacinta.Benchmarks.jsonBytes7) }

      bench(m"Parse file with Merino (tracking)")(target = 1*Second, operationSize = size7):
        '{ Json.Ast.parseTracked(jacinta.Benchmarks.jsonBytes7) }

      bench(m"Parse file with Jawn")(target = 1*Second, operationSize = size7):
        ' {
            import org.typelevel.jawn.ast.JParser
            JParser.parseFromString(jacinta.Benchmarks.jsonText7)
          }

      bench(m"Parse file with Circe")(target = 1*Second, operationSize = size7):
        '{ io.circe.parser.parse(jacinta.Benchmarks.jsonText7) }

      bench(m"Parse file with Jsoniter")(target = 1*Second, operationSize = size7):
        '{ jacinta.Benchmarks.parseWithJsoniter(jacinta.Benchmarks.jsonText7) }

      bench(m"Parse file with Jackson")(target = 1*Second, operationSize = size7):
        '{ jacinta.Benchmarks.parseWithJackson(jacinta.Benchmarks.jsonText7) }

    suite(m"Parse example 8 (1000 small decimals)"):
      val size8 = jsonBytes8.length*Byte

      bench(m"Parse file with Merino")
        ( target = 1*Second, operationSize = size8, baseline = Baseline(compare = Min) ):
        '{ Json.Ast.parse(jacinta.Benchmarks.jsonBytes8) }

      bench(m"Parse file with Merino (tracking)")(target = 1*Second, operationSize = size8):
        '{ Json.Ast.parseTracked(jacinta.Benchmarks.jsonBytes8) }

      bench(m"Parse file with Jawn")(target = 1*Second, operationSize = size8):
        ' {
            import org.typelevel.jawn.ast.JParser
            JParser.parseFromString(jacinta.Benchmarks.jsonText8)
          }

      bench(m"Parse file with Circe")(target = 1*Second, operationSize = size8):
        '{ io.circe.parser.parse(jacinta.Benchmarks.jsonText8) }

      bench(m"Parse file with Jsoniter")(target = 1*Second, operationSize = size8):
        '{ jacinta.Benchmarks.parseWithJsoniter(jacinta.Benchmarks.jsonText8) }

      bench(m"Parse file with Jackson")(target = 1*Second, operationSize = size8):
        '{ jacinta.Benchmarks.parseWithJackson(jacinta.Benchmarks.jsonText8) }

  lazy val jsonText1: String = jsonExample1.s
  lazy val jsonText2: String = jsonExample2.s
  lazy val jsonText3: String = jsonExample3.s
  lazy val jsonBytes1: Data = jsonText1.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val jsonBytes2: Data = jsonText2.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val jsonBytes3: Data = jsonText3.getBytes("UTF-8").nn.immutable(using Unsafe)

  // Example 4: array of 100 user records — the typical "JSON array of records"
  // pattern with a small fixed key set repeated across all elements.
  lazy val jsonText4: String =
    val sb = new _root_.java.lang.StringBuilder
    sb.append("{\"users\":[")
    var i = 0
    while i < 100 do
      if i > 0 then sb.append(',')
      val active = if (i & 1) == 0 then "true" else "false"
      val role = if i % 10 == 0 then "admin" else "user"
      sb.append(s"""{"id":$i,"username":"user$i","email":"user$i@example.com",""")
      sb.append(s"""\"active":$active,"role":"$role"}""")
      i += 1
    sb.append("]}")
    sb.toString.nn

  lazy val jsonBytes4: Data = jsonText4.getBytes("UTF-8").nn.immutable(using Unsafe)

  // Example 5: a longer NDJSON-style log array with 500 entries; 6 keys per
  // entry, all repeating across entries. Larger than Example 4 and stresses
  // the per-key path more heavily.
  lazy val jsonText5: String =
    val sb = new _root_.java.lang.StringBuilder
    sb.append("{\"logs\":[")
    val levels = Array("info", "debug", "warn", "error")
    val services = Array("auth", "api", "db", "cache", "worker")
    var i = 0
    while i < 500 do
      if i > 0 then sb.append(',')
      val ts = 1700000000L + i
      val level = levels(i & 3)
      val service = services(i % 5)
      val userId = 1000 + (i % 50)
      sb.append(s"""{"timestamp":$ts,"level":"$level","service":"$service",""")
      sb.append(s"""\"requestId":"req-$i","userId":$userId,"message":"event $i processed"}""")
      i += 1
    sb.append("]}")
    sb.toString.nn

  lazy val jsonBytes5: Data = jsonText5.getBytes("UTF-8").nn.immutable(using Unsafe)

  // Example 6: 50 blockchain-style transaction records exercising high-
  // precision numbers — wei values are 25-digit integers (overflowing
  // `Long`), gas prices are ~22-digit decimals, both well beyond the
  // 15-nibble fast path of `parseNumber`. Today these silently round to
  // `Double`; the future Array[Long] BCD work would preserve precision.
  lazy val jsonText6: String =
    val sb = new _root_.java.lang.StringBuilder
    sb.append("{\"transactions\":[")
    var i = 0
    while i < 50 do
      if i > 0 then sb.append(',')
      val nonce = i
      val blockNumber = 18500000 + i
      val gasUsed = 21000 + i*100
      // 25-digit integer (overflows Long ~19 digits)
      val valueWei = s"12345678901234567890${1000 + i}"
      // 22-digit decimal (overflows Double precision ~15-17 digits)
      val valueEth = s"1234567890.12345678901${i % 10}"
      // 22-digit decimal
      val gasPriceWei = s"30000000000.0123456789${i % 10}"
      // Scientific notation, ~20 sig figs
      val temperature = s"2.7345678901234567890${i % 10}e-3"
      sb.append(s"""{"from":"0xabcdef${i}","to":"0x123456${i}","value":$valueWei,""")
      sb.append(s"""\"valueEth":$valueEth,"gasPriceWei":$gasPriceWei,"gasUsed":$gasUsed,""")
      sb.append(s"""\"blockNumber":$blockNumber,"nonce":$nonce,"temperatureDelta":$temperature}""")
      i += 1
    sb.append("]}")
    sb.toString.nn

  lazy val jsonBytes6: Data = jsonText6.getBytes("UTF-8").nn.immutable(using Unsafe)

  // Pre-parsed AST of the blockchain corpus. Many of its number literals
  // overflow the parser's 14-nibble compact-BCD path and materialise as
  // `JsonBcd` values, so this exercises the printer's high-precision
  // number path on a realistic input rather than a microcase.
  lazy val blockchainAst: Json.Ast = unsafely(Json.Ast.parse(jsonBytes6))

  def printBlockchain(): Text =
    given Json.Formatting = Json.Formatting(Unset, false)
    blockchainAst.show

  // Example 7: a 1000-element array of small integers — the workload the
  // unboxed `Array[Long]` AST node was designed to accelerate (no per-
  // element boxing, no parity-padding sentinel).
  lazy val jsonText7: String =
    val sb = new _root_.java.lang.StringBuilder
    sb.append('[')
    var i = 0
    while i < 1000 do
      if i > 0 then sb.append(',')
      sb.append((i*37 + 1).toString)
      i += 1
    sb.append(']')
    sb.toString.nn

  lazy val jsonBytes7: Data = jsonText7.getBytes("UTF-8").nn.immutable(using Unsafe)

  // Example 8: 1000 small decimals — fractional values still fit the
  // compact-BCD payload, so the array stays in the unboxed form.
  lazy val jsonText8: String =
    val sb = new _root_.java.lang.StringBuilder
    sb.append('[')
    var i = 0
    while i < 1000 do
      if i > 0 then sb.append(',')
      sb.append(s"${i*7 + 1}.${(i*13 + 1) % 1000}")
      i += 1
    sb.append(']')
    sb.toString.nn

  lazy val jsonBytes8: Data = jsonText8.getBytes("UTF-8").nn.immutable(using Unsafe)

  val jsonExample1: Text = t"""

{"web-app": {
  "servlet": [
    {
      "servlet-name": "cofaxCDS",
      "servlet-class": "org.cofax.cds.CDSServlet",
      "init-param": {
        "configGlossary:installationAt": "Philadelphia, PA",
        "configGlossary:adminEmail": "ksm@pobox.com",
        "configGlossary:poweredBy": "Cofax",
        "configGlossary:poweredByIcon": "/images/cofax.gif",
        "configGlossary:staticPath": "/content/static",
        "templateProcessorClass": "org.cofax.WysiwygTemplate",
        "templateLoaderClass": "org.cofax.FilesTemplateLoader",
        "templatePath": "templates",
        "templateOverridePath": "",
        "defaultListTemplate": "listTemplate.htm",
        "defaultFileTemplate": "articleTemplate.htm",
        "useJSP": false,
        "jspListTemplate": "listTemplate.jsp",
        "jspFileTemplate": "articleTemplate.jsp",
        "cachePackageTagsTrack": 200,
        "cachePackageTagsStore": 200,
        "cachePackageTagsRefresh": 60,
        "cacheTemplatesTrack": 100,
        "cacheTemplatesStore": 50,
        "cacheTemplatesRefresh": 15,
        "cachePagesTrack": 200,
        "cachePagesStore": 100,
        "cachePagesRefresh": 10,
        "cachePagesDirtyRead": 10,
        "searchEngineListTemplate": "forSearchEnginesList.htm",
        "searchEngineFileTemplate": "forSearchEngines.htm",
        "searchEngineRobotsDb": "WEB-INF/robots.db",
        "useDataStore": true,
        "dataStoreClass": "org.cofax.SqlDataStore",
        "redirectionClass": "org.cofax.SqlRedirection",
        "dataStoreName": "cofax",
        "dataStoreDriver": "com.microsoft.jdbc.sqlserver.SQLServerDriver",
        "dataStoreUrl": "jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon",
        "dataStoreUser": "sa",
        "dataStorePassword": "dataStoreTestQuery",
        "dataStoreTestQuery": "SET NOCOUNT ON;select test='test';",
        "dataStoreLogFile": "/usr/local/tomcat/logs/datastore.log",
        "dataStoreInitConns": 10,
        "dataStoreMaxConns": 100,
        "dataStoreConnUsageLimit": 100,
        "dataStoreLogLevel": "debug",
        "maxUrlLength": 500}},
    {
      "servlet-name": "cofaxEmail",
      "servlet-class": "org.cofax.cds.EmailServlet",
      "init-param": {
      "mailHost": "mail1",
      "mailHostOverride": "mail2"}},
    {
      "servlet-name": "cofaxAdmin",
      "servlet-class": "org.cofax.cds.AdminServlet"},

    {
      "servlet-name": "fileServlet",
      "servlet-class": "org.cofax.cds.FileServlet"},
    {
      "servlet-name": "cofaxTools",
      "servlet-class": "org.cofax.cms.CofaxToolsServlet",
      "init-param": {
        "templatePath": "toolstemplates/",
        "log": 1,
        "logLocation": "/usr/local/tomcat/logs/CofaxTools.log",
        "logMaxSize": "",
        "dataLog": 1,
        "dataLogLocation": "/usr/local/tomcat/logs/dataLog.log",
        "dataLogMaxSize": "",
        "removePageCache": "/content/admin/remove?cache=pages&id=",
        "removeTemplateCache": "/content/admin/remove?cache=templates&id=",
        "fileTransferFolder": "/usr/local/tomcat/webapps/content/fileTransferFolder",
        "lookInContext": 1,
        "adminGroupID": 4,
        "betaServer": true}}],
  "servlet-mapping": {
    "cofaxCDS": "/",
    "cofaxEmail": "/cofaxutil/aemail/*",
    "cofaxAdmin": "/admin/*",
    "fileServlet": "/static/*",
    "cofaxTools": "/tools/*"},

  "taglib": {
    "taglib-uri": "cofax.tld",
    "taglib-location": "/WEB-INF/tlds/cofax.tld"}}}
"""

  val jsonExample2: Text = t"""
{"menu":{"id":"file","value":"File","popup":{"menuitem":[{"value":"New","onclick":"CreateNewDoc()"},
{"value":"Open","onclick":"OpenDoc()"},{"value":"Close","onclick":"CloseDoc()"}]}}}
"""

  val jsonExample3: Text = t"""
{"menu": {
  "header": "SVG Viewer",
    "items": [
        {"id": "Open"},
        {"id": "OpenNew", "label": "Open New"},
        null,
        {"id": "ZoomIn", "label": "Zoom In"},
        {"id": "ZoomOut", "label": "Zoom Out"},
        {"id": "OriginalView", "label": "Original View"},
        null,
        {"id": "Quality"},
        {"id": "Pause"},
        {"id": "Mute"},
        null,
        {"id": "Find", "label": "Find..."},
        {"id": "FindAgain", "label": "Find Again"},
        {"id": "Copy"},
        {"id": "CopyAgain", "label": "Copy Again"},
        {"id": "CopySVG", "label": "Copy SVG"},
        {"id": "ViewSVG", "label": "View SVG"},
        {"id": "ViewSource", "label": "View Source"},
        {"id": "SaveAs", "label": "Save As"},
        null,
        {"id": "Help"},
        {"id": "About", "label": "About Adobe CVG Viewer..."}
    ]
}}
"""
