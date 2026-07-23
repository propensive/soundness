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
package stratiform

import scala.language.unsafeNulls
import scala.quoted.*

import ambience.*, environments.javaEnvironment, systems.javaSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import hieroglyph.*, charEncoders.utf8Encoder
import probably.*
import proscenium.*
import quantitative.*
import rudiments.*
import sedentary.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import turbulence.*
import vacuous.*

// Parser-throughput benchmarks against TEL-converted versions of the
// eight JSON samples that the `jacinta.Benchmarks` suite uses. The
// JSON → TEL conversion (and the inferred Tels schema for each
// sample) are produced one-off by `etc/json-to-tel-bench.py` and
// committed under `lib/stratiform/res/bench/stratiform/`. The bench
// runs both the schemaless and the schema-aware parse path so the
// effect of the §19.5 schema-driven recovery overhead can be read
// directly off the table.

// The BinTEL decode corpus: an order book mirroring the crossparse corpus's
// shape, without its sum — a sum in field position derives to an
// unresolvable schema Reference, which BinTEL's AST path cannot decode
// either. Top-level, so the schema derivation and the generated parser see
// ordinary class symbols.
case class BLineItem(sku: Text, description: Text, quantity: Int, price: Double, taxed: Boolean)
case class BCustomer(id: Long, name: Text, email: Text, region: Text)

case class BOrder
  ( reference: Text, customer: BCustomer, items: List[BLineItem], priority: Boolean,
    discount: Double )

case class BOrders(orders: List[BOrder])

object Benchmarks extends Suite(m"Stratiform parser benchmarks"):
  sealed trait Information extends Dimension
  sealed trait Bytes[Power <: Nat] extends Units[Power, Information]
  val Byte: MetricUnit[Bytes[1]] = MetricUnit(1.0)

  given byteDesignation: Designation[Bytes[1]] = () => t"B"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice
  given prefixes:        Prefixes = Prefixes(List(Kilo, Mega, Giga, Tera))

  // Load a benchmark resource as raw UTF-8 bytes.
  private def loadBytes(name: String): Data =
    val stream = getClass.getResourceAsStream("/stratiform/" + name)
    if stream == null then sys.error("missing benchmark resource: " + name)
    val out = new _root_.java.io.ByteArrayOutputStream
    val buf = new Array[Byte](8192)
    var n = stream.read(buf)
    while n >= 0 do
      if n > 0 then out.write(buf, 0, n)
      n = stream.read(buf)

    stream.close()
    out.toByteArray.immutable(using Unsafe)

  // Load and parse a schema TEL document into a Tels via the
  // reconstructor — this is the schema the parser will use to drive
  // §19.5 recovery on the matching data file.
  private def loadSchema(name: String): Tels =
    val bytes = loadBytes(name)
    val tel = Tel.parse(bytes)
    Tels.Reconstructor.fromTel(tel)

  // Pre-load every example: the bench harness re-runs each `bench`
  // block thousands of times, so loading the resource per iteration
  // would skew the measurement towards the resource I/O.
  lazy val example1Bytes:  Data = loadBytes("example1.tel")
  lazy val example2Bytes:  Data = loadBytes("example2.tel")
  lazy val example3Bytes:  Data = loadBytes("example3.tel")
  lazy val example4Bytes:  Data = loadBytes("example4.tel")
  lazy val example5Bytes:  Data = loadBytes("example5.tel")
  lazy val example6Bytes:  Data = loadBytes("example6.tel")
  lazy val example7Bytes:  Data = loadBytes("example7.tel")
  lazy val example8Bytes:  Data = loadBytes("example8.tel")

  lazy val example1Schema: Tels = loadSchema("example1.schema.tel")
  lazy val example2Schema: Tels = loadSchema("example2.schema.tel")
  lazy val example3Schema: Tels = loadSchema("example3.schema.tel")
  lazy val example4Schema: Tels = loadSchema("example4.schema.tel")
  lazy val example5Schema: Tels = loadSchema("example5.schema.tel")
  lazy val example6Schema: Tels = loadSchema("example6.schema.tel")
  lazy val example7Schema: Tels = loadSchema("example7.schema.tel")
  lazy val example8Schema: Tels = loadSchema("example8.schema.tel")

  def lineItem(order: Int, item: Int): BLineItem =
    BLineItem
      ( sku         = t"SKU-$order-$item",
        description = t"component-${(order*7 + item*3) % 20}-assembly",
        quantity    = (order + item) % 9 + 1,
        price       = ((order*13 + item*7) % 400).toDouble + 0.25*(item % 4),
        taxed       = (order + item) % 2 == 0 )

  def order(index: Int): BOrder =
    val regions = List(t"north", t"south", t"east", t"west")

    BOrder
      ( reference = t"ORD-2026-${1000 + index}",
        customer  = BCustomer
          ( id     = 10000L + index,
            name   = t"customer-$index",
            email  = t"user$index@example.com",
            region = regions(index % 4) ),
        items     = List.tabulate(6)(lineItem(index, _)),
        priority  = index % 3 == 0,
        discount  = 0.25*(index % 3) )

  lazy val bintelCorpus: BOrders = BOrders(List.tabulate(12)(order(_)))
  lazy val bintelData: Data = bintelCorpus.bintel

  given bintelParsable: (BOrders is Bintel.Parsable) = BintelInlinable.parsable[BOrders]

  def decodeBintelAst(): BOrders = Bintel.read[BOrders](bintelData)
  def decodeBintelInlined(): BOrders = Bintel.parse[BOrders](bintelData)

  def run(): Unit =
    val bench = Bench()

    assert(decodeBintelInlined() == bintelCorpus, "BinTEL inlined decode disagrees")
    assert(decodeBintelAst() == bintelCorpus, "BinTEL AST decode disagrees")

    suite(m"Decode a BinTEL order corpus to case classes"):
      bench(m"BinTEL inlined")
        (target = 1*Second, warmups = 15):
        '{ stratiform.Benchmarks.decodeBintelInlined() }

      bench(m"BinTEL via AST")(target = 1*Second):
        '{ stratiform.Benchmarks.decodeBintelAst() }

    suite(m"Example 1 — web-app servlet config"):
      val size = example1Bytes.length*Byte

      bench(m"Parse (no schema)")
       ( target = 1*Second, operationSize = size ):
        '{ Tel.parse(stratiform.Benchmarks.example1Bytes) }

      bench(m"Parse (schema-aware)")(target = 1*Second, operationSize = size):
        ' {
            Tel.parse
             ( stratiform.Benchmarks.example1Bytes, stratiform.Benchmarks.example1Schema )
          }

    suite(m"Example 2 — small menu fragment"):
      val size = example2Bytes.length*Byte

      bench(m"Parse (no schema)")
       ( target = 1*Second, operationSize = size ):
        '{ Tel.parse(stratiform.Benchmarks.example2Bytes) }

      bench(m"Parse (schema-aware)")(target = 1*Second, operationSize = size):
        ' {
            Tel.parse
             ( stratiform.Benchmarks.example2Bytes, stratiform.Benchmarks.example2Schema )
          }

    suite(m"Example 3 — SVG viewer menu"):
      val size = example3Bytes.length*Byte

      bench(m"Parse (no schema)")
       ( target = 1*Second, operationSize = size ):
        '{ Tel.parse(stratiform.Benchmarks.example3Bytes) }

      bench(m"Parse (schema-aware)")(target = 1*Second, operationSize = size):
        ' {
            Tel.parse
             ( stratiform.Benchmarks.example3Bytes, stratiform.Benchmarks.example3Schema )
          }

    suite(m"Example 4 — 100 user records"):
      val size = example4Bytes.length*Byte

      bench(m"Parse (no schema)")
       ( target = 1*Second, operationSize = size ):
        '{ Tel.parse(stratiform.Benchmarks.example4Bytes) }

      bench(m"Parse (schema-aware)")(target = 1*Second, operationSize = size):
        ' {
            Tel.parse
             ( stratiform.Benchmarks.example4Bytes, stratiform.Benchmarks.example4Schema )
          }

    suite(m"Example 5 — 500 log entries"):
      val size = example5Bytes.length*Byte

      bench(m"Parse (no schema)")
       ( target = 1*Second, operationSize = size ):
        '{ Tel.parse(stratiform.Benchmarks.example5Bytes) }

      bench(m"Parse (schema-aware)")(target = 1*Second, operationSize = size):
        ' {
            Tel.parse
             ( stratiform.Benchmarks.example5Bytes, stratiform.Benchmarks.example5Schema )
          }

    suite(m"Example 6 — 50 high-precision blockchain transactions"):
      val size = example6Bytes.length*Byte

      bench(m"Parse (no schema)")
       ( target = 1*Second, operationSize = size ):
        '{ Tel.parse(stratiform.Benchmarks.example6Bytes) }

      bench(m"Parse (schema-aware)")(target = 1*Second, operationSize = size):
        ' {
            Tel.parse
             ( stratiform.Benchmarks.example6Bytes, stratiform.Benchmarks.example6Schema )
          }

    suite(m"Example 7 — 1000 small integers"):
      val size = example7Bytes.length*Byte

      bench(m"Parse (no schema)")
       ( target = 1*Second, operationSize = size ):
        '{ Tel.parse(stratiform.Benchmarks.example7Bytes) }

      bench(m"Parse (schema-aware)")(target = 1*Second, operationSize = size):
        ' {
            Tel.parse
             ( stratiform.Benchmarks.example7Bytes, stratiform.Benchmarks.example7Schema )
          }

    suite(m"Example 8 — 1000 small decimals"):
      val size = example8Bytes.length*Byte

      bench(m"Parse (no schema)")
       ( target = 1*Second, operationSize = size ):
        '{ Tel.parse(stratiform.Benchmarks.example8Bytes) }

      bench(m"Parse (schema-aware)")(target = 1*Second, operationSize = size):
        ' {
            Tel.parse
             ( stratiform.Benchmarks.example8Bytes, stratiform.Benchmarks.example8Schema )
          }
