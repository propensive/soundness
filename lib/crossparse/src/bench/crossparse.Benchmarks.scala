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

import ambience.*, environments.javaEnvironment, systems.javaSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import distillate.*
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import jacinta.*
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

  // ── The decode arms ───────────────────────────────────────────────────────
  // Each format reads from its natural whole-input form — JSON and TEL from
  // `Data`, XML and YAML from `Text` — with both of a format's arms reading
  // the *same* input kind, so direct-versus-AST is a fair comparison within
  // each format. YAML has no direct arm: ypsiloid's decoding is deliberately
  // AST-only (YAML aliases require materialized subtrees).
  def decodeJsonDirect(): Orders = jsonData.read[Orders in Json]
  def decodeJsonAst(): Orders = jsonData.read[Json].as[Orders]

  // The staged arm: monomorphic generated parsers for each record type,
  // hoisted so the KeyTables and instance arrays are built once. The nested
  // record types' staged givens are siblings, so each expansion finds them;
  // `items` (a `List`) and `payment` (a sum) still cross runtime `Json.Field`
  // seams — the current staged composition boundary.
  object staged:
    given lineItem: LineItem is Json.Parsable = Json.Parsable.staged
    given customer: Customer is Json.Parsable = Json.Parsable.staged
    given card: Payment.Card is Json.Parsable = Json.Parsable.staged
    given transfer: Payment.Transfer is Json.Parsable = Json.Parsable.staged
    given payment: Payment is Json.Parsable = Json.Parsable.staged
    given order: Order is Json.Parsable = Json.Parsable.staged
    given orders: Orders is Json.Parsable = Json.Parsable.staged

    given telLineItem: LineItem is Tel.Parsable = Tel.Parsable.staged
    given telCustomer: Customer is Tel.Parsable = Tel.Parsable.staged
    given telOrder: Order is Tel.Parsable = Tel.Parsable.staged
    given telOrders: Orders is Tel.Parsable = Tel.Parsable.staged

    given xmlLineItem: LineItem is Xml.Parsable = Xml.Parsable.staged
    given xmlCustomer: Customer is Xml.Parsable = Xml.Parsable.staged
    given xmlOrder: Order is Xml.Parsable = Xml.Parsable.staged
    given xmlOrders: Orders is Xml.Parsable = Xml.Parsable.staged

    // The Inlinable-composed parser: the whole instance graph (records,
    // collection loops, leaf parsers) resolves live inside an in-macro
    // staging compiler — the model component's companion givens — and
    // inlines into one flat parser. `payment` (a sum) has no Inlinable and
    // splices a runtime call to the sibling staged sum given above.
    given inlinedOrders: Orders is Json.Parsable = Inlinable.parsable[Orders]

  def decodeJsonStaged(): Orders =
    import staged.orders
    jsonData.read[Orders in Json]

  def decodeJsonInlined(): Orders =
    import staged.inlinedOrders
    jsonData.read[Orders in Json]

  def decodeTelStaged(): Orders =
    import staged.telOrders
    telData.read[Orders in Tel]

  def decodeXmlStaged(): Orders =
    import staged.xmlOrders
    xmlText.read[Orders in Xml]
  def decodeTelDirect(): Orders = telData.read[Orders in Tel]
  def decodeTelAst(): Orders = telData.read[Tel].as[Orders]
  def decodeXmlDirect(): Orders = xmlText.read[Orders in Xml]
  def decodeXmlAst(): Orders = xmlText.read[Xml].as[Orders]
  def decodeYamlAst(): Orders = yamlText.read[Yaml].as[Orders]

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
        + s"XML=${xmlText.s.length} YAML=${yamlText.s.length}")

    // The correctness gate: every arm must reproduce the original value
    // before anything is timed.
    assert(decodeJsonDirect() == corpus, "JSON direct decode disagrees with the corpus")
    assert(decodeJsonStaged() == corpus, "JSON staged decode disagrees with the corpus")
    assert(decodeJsonInlined() == corpus, "JSON inlined decode disagrees with the corpus")
    assert(decodeJsonAst() == corpus, "JSON AST decode disagrees with the corpus")
    assert(decodeTelDirect() == corpus, "TEL direct decode disagrees with the corpus")
    assert(decodeTelStaged() == corpus, "TEL staged decode disagrees with the corpus")
    assert(decodeTelAst() == corpus, "TEL AST decode disagrees with the corpus")
    assert(decodeXmlDirect() == corpus, "XML direct decode disagrees with the corpus")
    assert(decodeXmlStaged() == corpus, "XML staged decode disagrees with the corpus")
    assert(decodeXmlAst() == corpus, "XML AST decode disagrees with the corpus")
    assert(decodeYamlAst() == corpus, "YAML AST decode disagrees with the corpus")

    val jsoniterExpected = jsoniterMirror(corpus)
    assert(decodeJsoniterDirect() == jsoniterExpected, "Jsoniter direct decode disagrees")
    assert(decodeJsoniterAst() == jsoniterExpected, "Jsoniter AST decode disagrees")

    val bench = Bench()
    val profile = Profile()

    suite(m"Decode a ~10 KB order corpus to case classes"):
      bench(m"JSON direct")(target = 1*Second, baseline = Baseline(compare = Min)):
        '{ crossparse.Benchmarks.decodeJsonDirect() }

      bench(m"JSON staged")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeJsonStaged() }

      bench(m"JSON inlined")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeJsonInlined() }

      bench(m"JSON via AST")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeJsonAst() }

      bench(m"TEL direct")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeTelDirect() }

      bench(m"TEL staged")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeTelStaged() }

      bench(m"TEL via AST")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeTelAst() }

      bench(m"XML direct")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeXmlDirect() }

      bench(m"XML staged")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeXmlStaged() }

      bench(m"XML via AST")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeXmlAst() }

      // YAML decodes through the AST only: aliases require materialized
      // subtrees, so ypsiloid deliberately has no direct path.
      bench(m"YAML via AST")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeYamlAst() }

      bench(m"Jsoniter direct")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeJsoniterDirect() }

      bench(m"Jsoniter via AST")(target = 1*Second):
        '{ crossparse.Benchmarks.decodeJsoniterAst() }

    // Where the self-time actually goes in each direct arm — a JFR hotspot
    // histogram per parser, coloured by package. Jsoniter direct is the
    // reference. Longer targets so the sampler gathers enough execution samples.
    suite(m"Profile: direct-parser hotspots"):
      profile(m"TEL direct")(target = 5*Second):
        '{ crossparse.Benchmarks.decodeTelDirect() }

      profile(m"TEL staged")(target = 5*Second):
        '{ crossparse.Benchmarks.decodeTelStaged() }

      profile(m"XML direct")(target = 5*Second):
        '{ crossparse.Benchmarks.decodeXmlDirect() }

      profile(m"XML staged")(target = 5*Second):
        '{ crossparse.Benchmarks.decodeXmlStaged() }

      profile(m"JSON direct")(target = 5*Second):
        '{ crossparse.Benchmarks.decodeJsonDirect() }

      profile(m"JSON staged")(target = 5*Second):
        '{ crossparse.Benchmarks.decodeJsonStaged() }

      profile(m"JSON inlined")(target = 5*Second):
        '{ crossparse.Benchmarks.decodeJsonInlined() }

      profile(m"Jsoniter direct")(target = 5*Second):
        '{ crossparse.Benchmarks.decodeJsoniterDirect() }
