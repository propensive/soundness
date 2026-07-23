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
import superlunary.embeddings.automatic
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

enum JsonParser:
  case Merino, MerinoTracking, Jawn, Circe, Jsoniter, Jackson

enum Document:
  case Example1, Example2, Example3, Users, Logs, Integers, Decimals

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

  def decodeUsersStaged(): BenchUsers =
    given BenchUser is Json.Parsable = Json.Parsable.staged
    given BenchUsers is Json.Parsable = Json.Parsable.staged
    jsonBytes4.read[BenchUsers in Json]

  // ── Fused-tokenizer spike ─────────────────────────────────────────────────
  // Hand-written approximation of what a builder-parameterized staged
  // tokenizer would GENERATE for `BenchUsers`: one function, parser state in
  // locals only, bytes read directly, keys compared as packed-Long literals,
  // scans inlined. Happy-path only (bails by exception on anything the
  // generator would route to a slow path); exists purely to measure the
  // ceiling of fully-fused generated code before building the generator.
  object FusedSpike:
    private val words: java.lang.invoke.VarHandle =
      java.lang.invoke.MethodHandles
        . byteArrayViewVarHandle(Class.forName("[J").nn, java.nio.ByteOrder.LITTLE_ENDIAN)
        . nn

    private inline val HighBits = 0x8080808080808080L
    private inline val EveryByte = 0x0101010101010101L

    private def pack(name: String): Long =
      var low = 0L
      var index = 0

      while index < name.length do
        low |= (name.charAt(index).toLong & 0xFF) << (index*8)
        index += 1

      low

    private val UsersKey = pack("users")
    private val IdKey = pack("id")
    private val UsernameKey = pack("username")
    private val EmailKey = pack("email")
    private val ActiveKey = pack("active")
    private val RoleKey = pack("role")

    private def bail(): Nothing = sys.error("fused spike: unsupported input shape")

    def decode(buffer: Array[Byte]): BenchUsers =
      val limit = buffer.length

      inline def ws(p0: Int): Int =
        var p = p0

        while
          p < limit && {
            val b = buffer(p)
            b == ' ' || b == '\t' || b == '\n' || b == '\r'
          }
        do p += 1

        p

      // At the first content byte; the index of the closing quote.
      inline def scanString(p0: Int): Int =
        var p = p0
        var stop = -1

        while stop < 0 && p <= limit - 8 do
          val word: Long = words.get(buffer, p)
          val x22 = word ^ (0x22L*EveryByte)
          val x5c = word ^ (0x5CL*EveryByte)

          val stops =
            ((x22 - EveryByte) & ~x22 & HighBits)
              | ((x5c - EveryByte) & ~x5c & HighBits)
              | ((word - (0x20L*EveryByte)) & ~word & HighBits)
              | (word & HighBits)

          if stops == 0L then p += 8
          else
            p += java.lang.Long.numberOfTrailingZeros(stops) >> 3
            stop = p

        if stop < 0 then
          while p < limit && buffer(p) >= ' ' && buffer(p) != '"' && buffer(p) != '\\' do p += 1
          stop = p

        if stop >= limit || buffer(stop) != '"' then bail()
        stop

      // Key step: whitespace, separator (unless first), quote, key content,
      // quote, colon. Result packs the new position (high 32) with the key's
      // packed word truncated to 32 bits... keys here are <= 8 bytes, so the
      // packed low word is matched in full via the `key` local instead.
      var keyWord = 0L

      inline def keyStep(p0: Int, first: Boolean): Int =
        var p = ws(p0)
        if p >= limit then bail()

        if buffer(p) == '}' then
          keyWord = -1L
          p + 1
        else
          if !first then
            if buffer(p) != ',' then bail()
            p = ws(p + 1)

          if p >= limit || buffer(p) != '"' then bail()
          p += 1
          val start = p
          val end = scanString(p)
          val length = end - start
          if length == 0 || length > 8 then bail()
          val word: Long = words.get(buffer, start)
          keyWord = if length == 8 then word else word & ((1L << (length*8)) - 1)
          p = ws(end + 1)
          if p >= limit || buffer(p) != ':' then bail()
          p + 1

      var intValue = 0

      inline def parseInt(p0: Int): Int =
        var p = ws(p0)
        var negative = false

        if p < limit && buffer(p) == '-' then
          negative = true
          p += 1

        val start = p
        var value = 0

        while p < limit && buffer(p) >= '0' && buffer(p) <= '9' do
          value = value*10 + (buffer(p) - '0')
          p += 1

        if p == start || p - start > 9 then bail()
        intValue = if negative then -value else value
        p

      var stringValue: String = ""

      inline def parseString(p0: Int): Int =
        var p = ws(p0)
        if p >= limit || buffer(p) != '"' then bail()
        p += 1
        val end = scanString(p)
        stringValue = new String(buffer, p, end - p, java.nio.charset.StandardCharsets.ISO_8859_1)
        end + 1

      var booleanValue = false

      inline def parseBoolean(p0: Int): Int =
        val p = ws(p0)

        if p + 4 <= limit && buffer(p) == 't' && buffer(p + 1) == 'r' && buffer(p + 2) == 'u'
            && buffer(p + 3) == 'e'
        then
          booleanValue = true
          p + 4
        else if p + 5 <= limit && buffer(p) == 'f' && buffer(p + 1) == 'a'
            && buffer(p + 2) == 'l' && buffer(p + 3) == 's' && buffer(p + 4) == 'e'
        then
          booleanValue = false
          p + 5
        else bail()

      // ── the generated shape for BenchUsers begins here ──
      var p = ws(0)
      if p >= limit || buffer(p) != '{' then bail()
      p += 1

      var users: List[BenchUser] = Nil
      var usersSeen = false
      var firstKey = true
      var scanning = true

      while scanning do
        p = keyStep(p, firstKey)
        firstKey = false

        if keyWord == -1L then scanning = false
        else if keyWord == UsersKey then
          usersSeen = true
          p = ws(p)
          if p >= limit || buffer(p) != '[' then bail()
          p += 1
          val builder = List.newBuilder[BenchUser]
          p = ws(p)

          if p < limit && buffer(p) == ']' then p += 1
          else
            var elements = true

            while elements do
              // ── inlined generated shape for BenchUser ──
              p = ws(p)
              if p >= limit || buffer(p) != '{' then bail()
              p += 1

              var id = 0
              var username = ""
              var email = ""
              var active = false
              var role = ""
              var seen = 0
              var userFirst = true
              var inUser = true

              while inUser do
                p = keyStep(p, userFirst)
                userFirst = false

                if keyWord == -1L then inUser = false
                else if keyWord == IdKey then { p = parseInt(p); id = intValue; seen |= 1 }
                else if keyWord == UsernameKey then
                  p = parseString(p)
                  username = stringValue
                  seen |= 2
                else if keyWord == EmailKey then
                  p = parseString(p)
                  email = stringValue
                  seen |= 4
                else if keyWord == ActiveKey then
                  p = parseBoolean(p)
                  active = booleanValue
                  seen |= 8
                else if keyWord == RoleKey then
                  p = parseString(p)
                  role = stringValue
                  seen |= 16
                else bail()

              if seen != 31 then bail()
              builder += BenchUser(id, username.tt, email.tt, active, role.tt)
              p = ws(p)

              if p < limit && buffer(p) == ',' then p += 1
              else if p < limit && buffer(p) == ']' then
                p += 1
                elements = false
              else bail()

          users = builder.result()
        else bail()

      if !usersSeen then bail()
      BenchUsers(users)

  lazy val jsonArray4: Array[Byte] = jsonText4.getBytes("UTF-8").nn

  def decodeUsersFused(): BenchUsers = FusedSpike.decode(jsonArray4)

  val jsoniterUsersCodec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[JsoniterUsers] =
    com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker.make

  def decodeUsersJsoniter(): JsoniterUsers =
    com.github.plokhotnyuk.jsoniter_scala.core.readFromArray[JsoniterUsers]
      ( jsonBytes4.mutable(using Unsafe) )(using jsoniterUsersCodec)

  def textFor(document: Document): String = document match
    case Document.Example1 => jsonText1
    case Document.Example2 => jsonText2
    case Document.Example3 => jsonText3
    case Document.Users    => jsonText4
    case Document.Logs     => jsonText5
    case Document.Integers => jsonText7
    case Document.Decimals => jsonText8

  // Byte-array transport has no JSON codec, so Merino's cells receive the document as
  // text and re-encode it in the measuring JVM — through a single-slot cache keyed by
  // reference identity (the extracted document is memoized, so its identity is stable),
  // costing one comparison per iteration rather than one copy.
  private var utf8Key: String | Null = null
  private var utf8Value: Data = IArray[Byte]()

  def utf8(text: String): Data =
    if utf8Key ne text then
      utf8Key = text
      utf8Value = text.getBytes("UTF-8").nn.immutable(using Unsafe)

    utf8Value

  def run(): Unit =
    // The spike must agree with the production decoder before being timed.
    assert(decodeUsersFused() == decodeUsersDirectData(), "fused spike disagrees")

    val bench = Bench()

    val size4 = jsonBytes4.length*Byte
    val size6 = jsonBytes6.length*Byte

    // One benchmark, two axes: six parsers against seven documents, anchored to Merino.
    // Each document rides `References` as a spliced value: ONE staged tree per parser,
    // with seven dispatches each carrying a different document, extracted once per run.
    // operationSize is dropped: sizes vary per document and per-cell sizing isn't
    // supported yet.
    bench(m"Parse JSON documents")
      ( target = 1*Second, baseline = JsonParser.Merino, comparison = Baseline(compare = Min) )

    . over(JsonParser, Document):
        case (parser, document) =>
          val text: String = jacinta.Benchmarks.textFor(document)

          parser match
            case JsonParser.Merino =>
              '{ Json.Ast.parse(jacinta.Benchmarks.utf8($text)) }

            case JsonParser.MerinoTracking =>
              '{ Json.Ast.parseTracked(jacinta.Benchmarks.utf8($text)) }

            case JsonParser.Jawn =>
              ' {
                  import org.typelevel.jawn.ast.JParser
                  JParser.parseFromString($text)
                }

            case JsonParser.Circe =>
              '{ io.circe.parser.parse($text) }

            case JsonParser.Jsoniter =>
              '{ jacinta.Benchmarks.parseWithJsoniter($text) }

            case JsonParser.Jackson =>
              '{ jacinta.Benchmarks.parseWithJackson($text) }

    suite(m"Decode example 4 into records (100 user records)"):
      bench(m"Decode via the Json AST")(target = 1*Second, operationSize = size4):
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

      bench(m"Decode with a staged Parsable")(target = 1*Second, operationSize = size4):
        '{ jacinta.Benchmarks.decodeUsersStaged() }

      bench(m"Decode with a hand-fused parser (spike)")
        ( target = 1*Second, operationSize = size4 ):
        '{ jacinta.Benchmarks.decodeUsersFused() }

      bench(m"Decode directly with Jsoniter")(target = 1*Second, operationSize = size4):
        '{ jacinta.Benchmarks.decodeUsersJsoniter() }

    suite(m"Parse example 6 (50 high-precision blockchain transactions)"):
      // Three Merino rows exercising each `NumberMode` to visualise the
      // precision-vs-throughput trade-off on inputs that overflow the
      // in-Long fast path (every wei/gas value here is >15 nibbles).
      bench(m"Parse file with Merino (Full / Bcd Array[Double])")
        (target = 1*Second, operationSize = size6):
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
