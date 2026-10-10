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

import scala.sys

import scala.language.unsafeNulls
import scala.quoted.*

import ambience.*, environments.javaBaseEnvironment, systems.javaBaseSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import denominative.*
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import hieroglyph.*, codepages.utf8Codepage
import parasite.*, threads.virtualThreads, probates.cancelProbate
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
import zephyrine.*

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

// The serialization corpus: the 500-entry log document of `example5.tel`, as case classes, so
// that it can be encoded to a `Tel` whose atoms hold strings, as a server's response would.
case class BLog
  ( timestamp: Long, level: Text, service: Text, requestId: Text, userId: Int, message: Text )

case class BLogs(logs: List[BLog])

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
    val buf = new scala.Array[Byte](8192)
    var n = stream.read(buf)
    while n >= 0 do
      if n > 0 then out.write(buf, 0, n)
      n = stream.read(buf)

    stream.close()
    Array.unsafeFrozen(out.toByteArray.nn)

  // Load and parse a schema TEL document into a Tels via the
  // reconstructor — this is the schema the parser will use to drive
  // §19.5 recovery on the matching data file.
  private def loadSchema(name: String): Tels =
    val bytes = loadBytes(name)
    val tel = Tel.parse(bytes)
    tel.as[Tels]

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
            region = regions.stdlib(index % 4) ),
        items     = List.tabulate(6)(lineItem(index, _)),
        priority  = index % 3 == 0,
        discount  = 0.25*(index % 3) )

  lazy val bintelCorpus: BOrders = BOrders(List.tabulate(12)(order(_)))
  lazy val bintelData: Data = bintelCorpus.bintel

  given bintelParsable: (BOrders is Bintel.Parsable) = BintelInlinable.parsable[BOrders]

  // ── Serialization ──────────────────────────────────────────────────────
  //
  // Every arm writes the 500-entry log document to a discarding `OutputStream`, as a server
  // writing a response body would: once parsed from `example5.tel`, whose atoms are slices of
  // the parser's arena, and once encoded from case classes, whose atoms are strings. `show`
  // renders the whole text first; `emit` streams chunks from a fiber, or pushes them
  // synchronously as text or as UTF-8 bytes; and `lend` lends its own UTF-8 blocks.
  lazy val logsParsed: Tel = Tel.parse(example5Bytes)

  lazy val logsEncoded: Tel =
    import Tel.given
    val levels = scala.Array("info", "debug", "warn", "error")
    val services = scala.Array("auth", "api", "db", "cache", "worker")

    def log(index: Int): BLog =
      BLog
        ( 1700000000L + index, levels(index & 3).tt, services(index%5).tt,
          ("req-"+index).tt, 1000 + index%50, ("event "+index+" processed").tt )

    BLogs(List.tabulate(500)(log(_))).encode

  // Called by the agreement checks in `run()` as well as by the staged bodies.
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

    suite(m"Write 500 log entries, parsed from example5.tel, to an output stream"):
      val size = stratiform.Benchmarks.logsParsed.show.s.getBytes("UTF-8").nn.length*Byte

      bench(m"show, then write the whole text")(target = 1*Second, operationSize = size):
        '{
            val utf8 = java.nio.charset.StandardCharsets.UTF_8.nn
            val text = stratiform.Benchmarks.logsParsed.show.s
            java.io.OutputStream.nullOutputStream().nn.write(text.getBytes(utf8).nn)
        }

      bench(m"emit, streamed chunk by chunk")(target = 1*Second, operationSize = size):
        '{
            val out = java.io.OutputStream.nullOutputStream().nn
            val utf8 = java.nio.charset.StandardCharsets.UTF_8.nn

            supervise:
              Tel.emit(stratiform.Benchmarks.logsParsed).foreach: chunk =>
                out.write(chunk.s.getBytes(utf8).nn)
        }

      bench(m"emit, pushed synchronously")(target = 1*Second, operationSize = size):
        '{
            val out = java.io.OutputStream.nullOutputStream().nn
            val utf8 = java.nio.charset.StandardCharsets.UTF_8.nn
            val tel = stratiform.Benchmarks.logsParsed
            Tel.emit[Text](tel, chunk => out.write(chunk.s.getBytes(utf8).nn))
        }

      bench(m"emit, pushed as UTF-8 bytes")(target = 1*Second, operationSize = size):
        '{
            val out = java.io.OutputStream.nullOutputStream().nn
            val tel = stratiform.Benchmarks.logsParsed
            Tel.emit[Data](tel, chunk => out.write(chunk.asInstanceOf[scala.Array[Byte]]))
        }

      bench(m"lend, borrowed UTF-8 blocks")(target = 1*Second, operationSize = size):
        '{
            val out = java.io.OutputStream.nullOutputStream().nn

            Tel.lend(stratiform.Benchmarks.logsParsed): region =>
              interval =>
                val extent: Interval = interval
                val raw = unsafely(region.unsafeRaw.asInstanceOf[scala.Array[Byte]])
                out.write(raw, extent.start.n0, extent.size)
        }

    suite(m"Write 500 log entries, encoded from case classes, to an output stream"):
      val size = stratiform.Benchmarks.logsEncoded.show.s.getBytes("UTF-8").nn.length*Byte

      bench(m"show, then write the whole text")(target = 1*Second, operationSize = size):
        '{
            val utf8 = java.nio.charset.StandardCharsets.UTF_8.nn
            val text = stratiform.Benchmarks.logsEncoded.show.s
            java.io.OutputStream.nullOutputStream().nn.write(text.getBytes(utf8).nn)
        }

      bench(m"emit, streamed chunk by chunk")(target = 1*Second, operationSize = size):
        '{
            val out = java.io.OutputStream.nullOutputStream().nn
            val utf8 = java.nio.charset.StandardCharsets.UTF_8.nn

            supervise:
              Tel.emit(stratiform.Benchmarks.logsEncoded).foreach: chunk =>
                out.write(chunk.s.getBytes(utf8).nn)
        }

      bench(m"emit, pushed synchronously")(target = 1*Second, operationSize = size):
        '{
            val out = java.io.OutputStream.nullOutputStream().nn
            val utf8 = java.nio.charset.StandardCharsets.UTF_8.nn
            val tel = stratiform.Benchmarks.logsEncoded
            Tel.emit[Text](tel, chunk => out.write(chunk.s.getBytes(utf8).nn))
        }

      bench(m"emit, pushed as UTF-8 bytes")(target = 1*Second, operationSize = size):
        '{
            val out = java.io.OutputStream.nullOutputStream().nn
            val tel = stratiform.Benchmarks.logsEncoded
            Tel.emit[Data](tel, chunk => out.write(chunk.asInstanceOf[scala.Array[Byte]]))
        }

      bench(m"lend, borrowed UTF-8 blocks")(target = 1*Second, operationSize = size):
        '{
            val out = java.io.OutputStream.nullOutputStream().nn

            Tel.lend(stratiform.Benchmarks.logsEncoded): region =>
              interval =>
                val extent: Interval = interval
                val raw = unsafely(region.unsafeRaw.asInstanceOf[scala.Array[Byte]])
                out.write(raw, extent.start.n0, extent.size)
        }

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
