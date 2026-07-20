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
package caesura

import scala.language.unsafeNulls
import scala.quoted.*

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
import turbulence.*
import vacuous.*

object Benchmarks extends Suite(m"Caesura benchmarks"):
  sealed trait Information extends Dimension
  sealed trait Bytes[Power <: Nat] extends Units[Power, Information]
  val Byte: MetricUnit[Bytes[1]] = MetricUnit(1.0)

  given byteDesignation: Designation[Bytes[1]] = () => t"B"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice
  given prefixes:        Prefixes               = Prefixes(List(Kilo, Mega, Giga, Tera))

  // ─── fixtures ─────────────────────────────────────────────────────────────

  // Tiny: 10 rows × 5 numeric columns, no quoting. Per-call overhead.
  lazy val csvSmall: String =
    val sb = new _root_.java.lang.StringBuilder
    var i = 0
    while i < 10 do
      sb.append(i).append(',').append(i*2).append(',').append(i*3).append(',')
        .append(i*4).append(',').append(i*5).append('\n')
      i += 1
    sb.toString

  // Medium unquoted: 1 000 rows × 8 cols of mixed numeric/short text.
  lazy val csvMedium: String =
    val sb = new _root_.java.lang.StringBuilder
    var i = 0
    while i < 1000 do
      sb.append(i).append(',')
        .append("user").append(i).append(',')
        .append(i % 7).append(',')
        .append(1700000000L + i).append(',')
        .append(if (i & 1) == 0 then "true" else "false").append(',')
        .append("region-").append(i % 12).append(',')
        .append(i*1.25).append(',')
        .append("event_").append(i).append('\n')
      i += 1
    sb.toString

  // Medium quoted: 1 000 rows × 8 cols, every cell wrapped in "...". A handful
  // of cells contain embedded commas and "" escapes to exercise the slow path.
  lazy val csvMediumQuoted: String =
    val sb = new _root_.java.lang.StringBuilder
    var i = 0
    while i < 1000 do
      sb.append('"').append(i).append('"').append(',')
        .append('"').append("user, with comma ").append(i).append('"').append(',')
        .append('"').append(i % 7).append('"').append(',')
        .append('"').append(1700000000L + i).append('"').append(',')
        .append("\"a \"\"quoted\"\" word\"").append(',')
        .append('"').append("region-").append(i % 12).append('"').append(',')
        .append('"').append(i*1.25).append('"').append(',')
        .append('"').append("event_").append(i).append('"').append('\n')
      i += 1
    sb.toString

  // Large unquoted: 100 000 rows × 10 cols mixed. Pure throughput.
  lazy val csvLarge: String =
    val sb = new _root_.java.lang.StringBuilder(12 * 1024 * 1024)
    var i = 0
    while i < 100000 do
      sb.append(i).append(',')
        .append("user").append(i).append(',')
        .append(i % 7).append(',')
        .append(1700000000L + i).append(',')
        .append(if (i & 1) == 0 then "true" else "false").append(',')
        .append("region-").append(i % 12).append(',')
        .append(i*1.25).append(',')
        .append("event_").append(i).append(',')
        .append("category-").append(i % 50).append(',')
        .append((i*7919L)%100003L).append('\n')
      i += 1
    sb.toString

  // Medium TSV: same shape as csvMedium but tab-delimited, no quoting.
  lazy val tsvMedium: String =
    val sb = new _root_.java.lang.StringBuilder
    var i = 0
    while i < 1000 do
      sb.append(i).append('\t')
        .append("user").append(i).append('\t')
        .append(i % 7).append('\t')
        .append(1700000000L + i).append('\t')
        .append(if (i & 1) == 0 then "true" else "false").append('\t')
        .append("region-").append(i % 12).append('\t')
        .append(i*1.25).append('\t')
        .append("event_").append(i).append('\n')
      i += 1
    sb.toString

  // Text views (for Caesura).
  lazy val csvSmallText:        Text = Text(csvSmall)
  lazy val csvMediumText:       Text = Text(csvMedium)
  lazy val csvMediumQuotedText: Text = Text(csvMediumQuoted)
  lazy val csvLargeText:        Text = Text(csvLarge)
  lazy val tsvMediumText:       Text = Text(tsvMedium)

  // ─── Caesura helpers ──────────────────────────────────────────────────────

  def caesuraParseCsv(text: Text): Int =
    import dsvFormats.csvFormat
    var n = 0
    text.read[Sheet].rows.foreach { _ => n += 1 }
    n

  def caesuraParseTsv(text: Text): Int =
    import dsvFormats.tsvFormat
    var n = 0
    text.read[Sheet].rows.foreach { _ => n += 1 }
    n

  // ─── Univocity helpers ────────────────────────────────────────────────────

  def univocityParseCsv(text: String): Int =
    val settings = new com.univocity.parsers.csv.CsvParserSettings()
    settings.getFormat.setLineSeparator("\n")
    val parser = new com.univocity.parsers.csv.CsvParser(settings)
    val reader = new _root_.java.io.StringReader(text)
    parser.beginParsing(reader)
    var n = 0
    var row: Array[String] = parser.parseNext()
    while row != null do
      n += 1
      row = parser.parseNext()
    parser.stopParsing()
    n

  def univocityParseTsv(text: String): Int =
    val settings = new com.univocity.parsers.tsv.TsvParserSettings()
    settings.getFormat.setLineSeparator("\n")
    val parser = new com.univocity.parsers.tsv.TsvParser(settings)
    val reader = new _root_.java.io.StringReader(text)
    parser.beginParsing(reader)
    var n = 0
    var row: Array[String] = parser.parseNext()
    while row != null do
      n += 1
      row = parser.parseNext()
    parser.stopParsing()
    n

  // ─── OpenCSV helpers ──────────────────────────────────────────────────────

  def openCsvParseCsv(text: String): Int =
    val reader = new com.opencsv.CSVReader(new _root_.java.io.StringReader(text))
    var n = 0
    var row: Array[String] = reader.readNext()
    while row != null do
      n += 1
      row = reader.readNext()
    reader.close()
    n

  def openCsvParseTsv(text: String): Int =
    val parser = new com.opencsv.CSVParserBuilder().withSeparator('\t').build()
    val reader = new com.opencsv.CSVReaderBuilder(new _root_.java.io.StringReader(text))
      .withCSVParser(parser).build()
    var n = 0
    var row: Array[String] = reader.readNext()
    while row != null do
      n += 1
      row = reader.readNext()
    reader.close()
    n

  // ─── Apache Commons CSV helpers ───────────────────────────────────────────

  def commonsParseCsv(text: String): Int =
    val records = org.apache.commons.csv.CSVFormat.DEFAULT
      .parse(new _root_.java.io.StringReader(text))
    val it = records.iterator()
    var n = 0
    while it.hasNext do { it.next(); n += 1 }
    records.close()
    n

  def commonsParseTsv(text: String): Int =
    val records = org.apache.commons.csv.CSVFormat.TDF
      .parse(new _root_.java.io.StringReader(text))
    val it = records.iterator()
    var n = 0
    while it.hasNext do { it.next(); n += 1 }
    records.close()
    n

  // ─── FastCSV helpers ──────────────────────────────────────────────────────

  def fastCsvParseCsv(text: String): Int =
    val reader = de.siegmar.fastcsv.reader.CsvReader.builder()
      .ofCsvRecord(new _root_.java.io.StringReader(text))
    val it = reader.iterator()
    var n = 0
    while it.hasNext do { it.next(); n += 1 }
    reader.close()
    n

  def fastCsvParseTsv(text: String): Int =
    val reader = de.siegmar.fastcsv.reader.CsvReader.builder()
      .fieldSeparator('\t')
      .ofCsvRecord(new _root_.java.io.StringReader(text))
    val it = reader.iterator()
    var n = 0
    while it.hasNext do { it.next(); n += 1 }
    reader.close()
    n

  // ─── benchmarks ───────────────────────────────────────────────────────────

  def run(): Unit =
    val bench = Bench()

    val sizeSmall:        Quantity[Bytes[1]] = csvSmall.length*Byte
    val sizeMedium:       Quantity[Bytes[1]] = csvMedium.length*Byte
    val sizeMediumQuoted: Quantity[Bytes[1]] = csvMediumQuoted.length*Byte
    val sizeLarge:        Quantity[Bytes[1]] = csvLarge.length*Byte
    val sizeTsv:          Quantity[Bytes[1]] = tsvMedium.length*Byte

    suite(m"Tiny CSV (10 rows × 5 numeric cols, no quoting)"):
      bench(m"Caesura")
        ( target = 1*Second, operationSize = sizeSmall, baseline = Baseline(compare = Min) ):
        '{ caesura.Benchmarks.caesuraParseCsv(caesura.Benchmarks.csvSmallText) }

      bench(m"Univocity")(target = 1*Second, operationSize = sizeSmall):
        '{ caesura.Benchmarks.univocityParseCsv(caesura.Benchmarks.csvSmall) }

      bench(m"OpenCSV")(target = 1*Second, operationSize = sizeSmall):
        '{ caesura.Benchmarks.openCsvParseCsv(caesura.Benchmarks.csvSmall) }

      bench(m"Commons CSV")(target = 1*Second, operationSize = sizeSmall):
        '{ caesura.Benchmarks.commonsParseCsv(caesura.Benchmarks.csvSmall) }

      bench(m"FastCSV")(target = 1*Second, operationSize = sizeSmall):
        '{ caesura.Benchmarks.fastCsvParseCsv(caesura.Benchmarks.csvSmall) }

    suite(m"Medium CSV (1 000 rows × 8 cols, no quoting)"):
      bench(m"Caesura")
        ( target = 1*Second, operationSize = sizeMedium, baseline = Baseline(compare = Min) ):
        '{ caesura.Benchmarks.caesuraParseCsv(caesura.Benchmarks.csvMediumText) }

      bench(m"Univocity")(target = 1*Second, operationSize = sizeMedium):
        '{ caesura.Benchmarks.univocityParseCsv(caesura.Benchmarks.csvMedium) }

      bench(m"OpenCSV")(target = 1*Second, operationSize = sizeMedium):
        '{ caesura.Benchmarks.openCsvParseCsv(caesura.Benchmarks.csvMedium) }

      bench(m"Commons CSV")(target = 1*Second, operationSize = sizeMedium):
        '{ caesura.Benchmarks.commonsParseCsv(caesura.Benchmarks.csvMedium) }

      bench(m"FastCSV")(target = 1*Second, operationSize = sizeMedium):
        '{ caesura.Benchmarks.fastCsvParseCsv(caesura.Benchmarks.csvMedium) }

    suite(m"Medium CSV (1 000 rows × 8 cols, fully quoted with embedded commas/quotes)"):
      bench(m"Caesura")
        ( target = 1*Second, operationSize = sizeMediumQuoted, baseline = Baseline(compare = Min) ):
        '{ caesura.Benchmarks.caesuraParseCsv(caesura.Benchmarks.csvMediumQuotedText) }

      bench(m"Univocity")(target = 1*Second, operationSize = sizeMediumQuoted):
        '{ caesura.Benchmarks.univocityParseCsv(caesura.Benchmarks.csvMediumQuoted) }

      bench(m"OpenCSV")(target = 1*Second, operationSize = sizeMediumQuoted):
        '{ caesura.Benchmarks.openCsvParseCsv(caesura.Benchmarks.csvMediumQuoted) }

      bench(m"Commons CSV")(target = 1*Second, operationSize = sizeMediumQuoted):
        '{ caesura.Benchmarks.commonsParseCsv(caesura.Benchmarks.csvMediumQuoted) }

      bench(m"FastCSV")(target = 1*Second, operationSize = sizeMediumQuoted):
        '{ caesura.Benchmarks.fastCsvParseCsv(caesura.Benchmarks.csvMediumQuoted) }

    suite(m"Large CSV (100 000 rows × 10 cols, no quoting)"):
      bench(m"Caesura")
        ( target = 1*Second, operationSize = sizeLarge, baseline = Baseline(compare = Min) ):
        '{ caesura.Benchmarks.caesuraParseCsv(caesura.Benchmarks.csvLargeText) }

      bench(m"Univocity")(target = 1*Second, operationSize = sizeLarge):
        '{ caesura.Benchmarks.univocityParseCsv(caesura.Benchmarks.csvLarge) }

      bench(m"OpenCSV")(target = 1*Second, operationSize = sizeLarge):
        '{ caesura.Benchmarks.openCsvParseCsv(caesura.Benchmarks.csvLarge) }

      bench(m"Commons CSV")(target = 1*Second, operationSize = sizeLarge):
        '{ caesura.Benchmarks.commonsParseCsv(caesura.Benchmarks.csvLarge) }

      bench(m"FastCSV")(target = 1*Second, operationSize = sizeLarge):
        '{ caesura.Benchmarks.fastCsvParseCsv(caesura.Benchmarks.csvLarge) }

    suite(m"Medium TSV (1 000 rows × 8 cols, tab-delimited)"):
      bench(m"Caesura")
        ( target = 1*Second, operationSize = sizeTsv, baseline = Baseline(compare = Min) ):
        '{ caesura.Benchmarks.caesuraParseTsv(caesura.Benchmarks.tsvMediumText) }

      bench(m"Univocity")(target = 1*Second, operationSize = sizeTsv):
        '{ caesura.Benchmarks.univocityParseTsv(caesura.Benchmarks.tsvMedium) }

      bench(m"OpenCSV")(target = 1*Second, operationSize = sizeTsv):
        '{ caesura.Benchmarks.openCsvParseTsv(caesura.Benchmarks.tsvMedium) }

      bench(m"Commons CSV")(target = 1*Second, operationSize = sizeTsv):
        '{ caesura.Benchmarks.commonsParseTsv(caesura.Benchmarks.tsvMedium) }

      bench(m"FastCSV")(target = 1*Second, operationSize = sizeTsv):
        '{ caesura.Benchmarks.fastCsvParseTsv(caesura.Benchmarks.tsvMedium) }
