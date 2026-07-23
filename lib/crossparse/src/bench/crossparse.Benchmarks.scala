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
package crossparse

import scala.sys

import ambience.*, environments.javaEnvironment, systems.javaSystem
import anticipation.*
import breviloquence.*
import contingency.*, strategies.throwUnsafely
import distillate.*
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import jacinta.*
import locomotion.*
import prepositional.*
import probably.*
import proscenium.*
import quantitative.*
import rudiments.*
import sedentary.*
import spectacular.*
import stratiform.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import turbulence.*
import vacuous.*
import wisteria.*
import xylophone.*
import ypsiloid.*

// A discriminator key inside the value, mirroring the format tests: JSON
// carries `"kind": …`, YAML carries `type: …`; TEL and XML need no import
// (TEL keys the variant compound by name; XML uses the custom `Discriminable`
// on `Payment`'s companion below).
import jacinta.discriminables.jsonByKindDiscriminable
import jacinta.formatting.compactJsonFormatting
import ypsiloid.discriminables.yamlByTypeDiscriminable
import ypsiloid.formatting.blockYamlFormatting

// Ypsiloid's `Yaml is Showable` is a top-level given (not a companion
// member), so it must be imported by name.
import ypsiloid.showable

// Jsoniter/circe mirrors of the corpus structure, with `String` fields as
// those libraries expect; the `kind` discriminator matches the JSON corpus.
sealed trait JsoniterPayment derives io.circe.derivation.ConfiguredDecoder

object JsoniterPayment:
  case class Card(number: String, expiry: String, secure: Boolean) extends JsoniterPayment
  case class Transfer(iban: String, reference: Long) extends JsoniterPayment

case class JsoniterLineItem
  (sku: String, description: String, quantity: Int, price: Double, taxed: Boolean)
derives io.circe.derivation.ConfiguredDecoder

case class JsoniterCustomer(id: Long, name: String, email: String, region: String)
derives io.circe.derivation.ConfiguredDecoder

case class JsoniterOrder
  ( reference: String, customer: JsoniterCustomer, items: List[JsoniterLineItem],
    payment: JsoniterPayment, priority: Boolean, discount: Double )
derives io.circe.derivation.ConfiguredDecoder

case class JsoniterOrders(orders: List[JsoniterOrder])
derives io.circe.derivation.ConfiguredDecoder

// The circe derivation reads this at each `derives` site above.
given io.circe.derivation.Configuration =
  io.circe.derivation.Configuration.default.withDiscriminator("kind")

object Benchmarks extends Suite(m"Cross-format direct-parsing benchmarks"):
  given decimalizer: Decimalizer = Decimalizer(2)
  given device: BenchmarkDevice = LocalhostDevice
  given schema: XmlSchema = XmlSchema.Freeform

  // ── The corpus ────────────────────────────────────────────────────────────
  // One deterministic `Orders` value, sized so its JSON serialization is
  // ~10 KB, then serialized once through each format's own encoder so that
  // every arm decodes semantically identical data. Doubles are multiples of
  // 0.25 (exactly representable) and `Text` values avoid spaces, so every
  // format round-trips values byte-stably.
  def lineItem(order: Int, item: Int): LineItem =
    LineItem
      ( sku         = t"SKU-$order-$item",
        description = t"component-${(order*7 + item*3) % 20}-assembly",
        quantity    = (order + item) % 9 + 1,
        price       = ((order*13 + item*7) % 400).toDouble + 0.25*(item % 4),
        taxed       = (order + item) % 2 == 0 )

  def order(index: Int): Order =
    val regions = List(t"north", t"south", t"east", t"west")

    val payment: Payment =
      if index % 2 == 0
      then Payment.Card
        ( number = t"4000-0000-0000-${1000 + index}",
          expiry = t"20${26 + index % 4}-0${index % 9 + 1}",
          secure = index % 3 == 0 )
      else Payment.Transfer
        ( iban      = t"GB${10 + index}BANK${700000 + index*13}",
          reference = 500000L + index*37 )

    Order
      ( reference = t"ORD-2026-${1000 + index}",
        customer  = Customer
          ( id     = 10000L + index,
            name   = t"customer-$index",
            email  = t"user$index@example.com",
            region = regions(index % 4) ),
        items     = List.tabulate(6)(lineItem(index, _)),
        payment   = payment,
        priority  = index % 3 == 0,
        discount  = 0.25*(index % 3) )

  val corpus: Orders = Orders(List.tabulate(12)(order(_)))

  lazy val jsonText: Text = corpus.in[Json].show
  lazy val telText: Text = corpus.in[Tel].show
  lazy val xmlText: Text = corpus.in[Xml].show
  lazy val yamlText: Text = corpus.in[Yaml].show

  lazy val jsonData: Data = jsonText.s.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val telData: Data = telText.s.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val cborData: Data = Cbor.Ast.encodable.encoded(Cbor.unseal(corpus.in[Cbor]))
  lazy val protobufData: Data = corpus.in[Protobuf].encode

  // ── The decode arms ───────────────────────────────────────────────────────
  // Each format is measured two ways: through its materialized AST, and
  // through its inlined parser — the Expr-composed, monomorphic decoder each
  // format's `Inlinable.parsable` generates. Both of a format's arms read
  // the *same* input kind (JSON and TEL from `Data`, XML and YAML from
  // `Text`), so AST-versus-inlined is a fair comparison within each format.
  // YAML has no inlined arm: ypsiloid's decoding is deliberately AST-only
  // (YAML aliases require materialized subtrees).
  def decodeJsonAst(): Orders = jsonData.read[Json].as[Orders]

  object inlined:
    // The JSON sum bridge: `payment` has no `Inlinable`, so the inlined
    // parser binds this sibling staged sum through its runtime seam; the
    // variant instances are its staged siblings.
    given card: Payment.Card is Json.Parsable = Json.Parsable.staged
    given transfer: Payment.Transfer is Json.Parsable = Json.Parsable.staged
    given payment: Payment is Json.Parsable = Json.Parsable.staged

    // The Inlinable-composed parsers: the whole instance graph (records,
    // collection gathering, leaf parsers — and, for TEL and XML, the sum
    // itself) resolves live inside an in-macro staging compiler — the model
    // component's companion givens — and inlines into one flat parser per
    // format.
    given inlinedOrders: Orders is Json.Parsable = jacinta.Inlinable.parsable[Orders]
    given inlinedTelOrders: Orders is Tel.Parsable = stratiform.Inlinable.parsable[Orders]
    given inlinedXmlOrders: Orders is Xml.Parsable = xylophone.Inlinable.parsable[Orders]
    given inlinedCborOrders: Orders is Cbor.Parsable = breviloquence.Inlinable.parsable[Orders]

    given inlinedProtobufOrders: Orders is Protobuf.Parsable =
      locomotion.Inlinable.parsable[Orders]

  def decodeJsonInlined(): Orders =
    import inlined.inlinedOrders
    jsonData.read[Orders in Json]

  def decodeTelInlined(): Orders =
    import inlined.inlinedTelOrders
    telData.read[Orders in Tel]

  def decodeXmlInlined(): Orders =
    import inlined.inlinedXmlOrders
    xmlText.read[Orders in Xml]

  def decodeCborInlined(): Orders =
    import inlined.inlinedCborOrders
    cborData.read[Orders in Cbor]

  def decodeProtobufInlined(): Orders =
    import inlined.inlinedProtobufOrders
    protobufData.read[Orders in Protobuf]

  def decodeTelAst(): Orders = telData.read[Tel].as[Orders]
  def decodeXmlAst(): Orders = xmlText.read[Xml].as[Orders]
  def decodeYamlAst(): Orders = yamlText.read[Yaml].as[Orders]
  def decodeCborAst(): Orders = cborData.read[Cbor].as[Orders]
  def decodeProtobufAst(): Orders = protobufData.read[Protobuf].as[Orders]

  // ── Jsoniter arms ─────────────────────────────────────────────────────────
  // The external yardstick, decoding the same JSON corpus: direct with a
  // macro-generated codec (configured for the corpus's `kind` discriminator),
  // and via an AST — Jsoniter parsing into a circe `Json` tree, then a circe
  // decoder walking it, the analog of the other formats' AST arms.
  val jsoniterOrdersCodec
  :   com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[JsoniterOrders] =
    com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker.make
      ( com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig
          . withDiscriminatorFieldName(Some("kind"))
          // The corpus writes the discriminator last, as Jacinta does.
          . withRequireDiscriminatorFirst(false) )

  val circeAstCodec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[io.circe.Json] =
    com.github.plokhotnyuk.jsoniter_scala.circe.JsoniterScalaCodec.jsonCodec()

  def decodeJsoniterDirect(): JsoniterOrders =
    com.github.plokhotnyuk.jsoniter_scala.core.readFromArray[JsoniterOrders]
      ( jsonData.mutable(using Unsafe) )(using jsoniterOrdersCodec)

  def decodeJsoniterAst(): JsoniterOrders =
    val ast = com.github.plokhotnyuk.jsoniter_scala.core.readFromArray[io.circe.Json]
      ( jsonData.mutable(using Unsafe) )(using circeAstCodec)

    summon[io.circe.Decoder[JsoniterOrders]].decodeJson(ast) match
      case Right(orders) => orders
      case Left(error)   => sys.error(error.getMessage.nn)

  // The expected value for the Jsoniter correctness gate.
  def jsoniterMirror(orders: Orders): JsoniterOrders =
    JsoniterOrders
      ( orders.orders.map { order =>
          JsoniterOrder
            ( order.reference.s,
              JsoniterCustomer
                ( order.customer.id, order.customer.name.s, order.customer.email.s,
                  order.customer.region.s ),
              order.items.map { item =>
                JsoniterLineItem
                  (item.sku.s, item.description.s, item.quantity, item.price, item.taxed) },
              order.payment match
                case Payment.Card(number, expiry, secure) =>
                  JsoniterPayment.Card(number.s, expiry.s, secure)

                case Payment.Transfer(iban, reference) =>
                  JsoniterPayment.Transfer(iban.s, reference),
              order.priority,
              order.discount ) } )

  def run(): Unit =
    println(s"Corpus sizes (bytes): JSON=${jsonText.s.length} TEL=${telText.s.length} "
        + s"XML=${xmlText.s.length} YAML=${yamlText.s.length} CBOR=${cborData.length} "
        + s"Protobuf=${protobufData.length}")

    // The correctness gate: every arm must reproduce the original value
    // before anything is timed.
    assert(decodeJsonInlined() == corpus, "JSON inlined decode disagrees with the corpus")
    assert(decodeJsonAst() == corpus, "JSON AST decode disagrees with the corpus")
    assert(decodeTelInlined() == corpus, "TEL inlined decode disagrees with the corpus")
    assert(decodeTelAst() == corpus, "TEL AST decode disagrees with the corpus")
    assert(decodeXmlInlined() == corpus, "XML inlined decode disagrees with the corpus")
    assert(decodeXmlAst() == corpus, "XML AST decode disagrees with the corpus")
    assert(decodeYamlAst() == corpus, "YAML AST decode disagrees with the corpus")
    assert(decodeCborInlined() == corpus, "CBOR inlined decode disagrees with the corpus")
    assert(decodeCborAst() == corpus, "CBOR AST decode disagrees with the corpus")
    assert(decodeProtobufInlined() == corpus, "Protobuf inlined decode disagrees with the corpus")
    assert(decodeProtobufAst() == corpus, "Protobuf AST decode disagrees with the corpus")

    matrixConfig.check()
    matrixMenu.check()
    matrixUsers.check()
    matrixLogs.check()
    matrixTransactions.check()
    matrixInts.check()
    matrixDecimals.check()

    val jsoniterExpected = jsoniterMirror(corpus)
    assert(decodeJsoniterDirect() == jsoniterExpected, "Jsoniter direct decode disagrees")
    assert(decodeJsoniterAst() == jsoniterExpected, "Jsoniter AST decode disagrees")

    val bench = Bench()
    val profile = Profile()

    suite(m"Decode a ~10 KB order corpus to case classes"):
      // The suite's first row absorbs the JVM's compile ramp, so it warms
      // longer than the rest.
      bench(m"JSON inlined")
        (target = 1*Second, warmups = 15, baseline = Baseline(compare = Min)):
        '{ crossparse.Benchmarks.decodeJsonInlined() }

      bench(m"JSON via AST")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeJsonAst() }

      bench(m"TEL inlined")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeTelInlined() }

      bench(m"TEL via AST")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeTelAst() }

      bench(m"XML inlined")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeXmlInlined() }

      bench(m"XML via AST")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeXmlAst() }

      bench(m"CBOR inlined")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeCborInlined() }

      bench(m"CBOR via AST")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeCborAst() }

      bench(m"Protobuf inlined")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeProtobufInlined() }

      bench(m"Protobuf via AST")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeProtobufAst() }

      // YAML decodes through the AST only: aliases require materialized
      // subtrees, so ypsiloid deliberately has no inlined path.
      bench(m"YAML via AST")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeYamlAst() }

      bench(m"Jsoniter direct")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeJsoniterDirect() }

      bench(m"Jsoniter via AST")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeJsoniterAst() }


    matrixConfig.suites(bench)

    matrixMenu.suites(bench)

    matrixUsers.suites(bench)

    matrixLogs.suites(bench)

    matrixTransactions.suites(bench)

    matrixInts.suites(bench)

    matrixDecimals.suites(bench)

    // Where the self-time actually goes in each inlined arm — a JFR hotspot
    // histogram per parser, coloured by package. Jsoniter direct is the
    // reference. Longer targets so the sampler gathers enough execution samples.
    suite(m"Profile: inlined-parser hotspots"):
      profile(m"TEL inlined")(target = 5*Second):
        '{ crossparse.Benchmarks.decodeTelInlined() }

      profile(m"CBOR inlined")(target = 5*Second):
        '{ crossparse.Benchmarks.decodeCborInlined() }

      profile(m"Protobuf inlined")(target = 5*Second):
        '{ crossparse.Benchmarks.decodeProtobufInlined() }

      profile(m"XML inlined")(target = 5*Second):
        '{ crossparse.Benchmarks.decodeXmlInlined() }

      profile(m"JSON inlined")(target = 5*Second):
        '{ crossparse.Benchmarks.decodeJsonInlined() }

      profile(m"Jsoniter direct")(target = 5*Second):
        '{ crossparse.Benchmarks.decodeJsoniterDirect() }
