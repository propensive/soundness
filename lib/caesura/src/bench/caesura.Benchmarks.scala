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
import superlunary.embeddings.automatic
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import turbulence.*
import vacuous.*

enum Library:
  case Caesura, Univocity, OpenCsv, CommonsCsv, FastCsv

enum Corpus:
  case Tiny, Medium, MediumQuoted, Large, Tsv

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

  def documentFor(corpus: Corpus): String = corpus match
    case Corpus.Tiny         => csvSmall
    case Corpus.Medium       => csvMedium
    case Corpus.MediumQuoted => csvMediumQuoted
    case Corpus.Large        => csvLarge
    case Corpus.Tsv          => tsvMedium

  def run(): Unit =
    val bench = Bench()

    // One benchmark, two axes: five libraries against five corpora, anchored to Caesura,
    // rendered as a grid of mean times followed by a grid of relative rates per corpus.
    // Each corpus rides `References` as a spliced value: two staged trees per library (the
    // TSV column parses through a different entry point), extracted once per run.
    bench(m"Parse DSV documents")
      ( target = 1*Second, baseline = Library.Caesura, comparison = Baseline(compare = Min) )

    . over(Library, Corpus):
        case (library, corpus) =>
          val document: Text = Text(caesura.Benchmarks.documentFor(corpus))
          val tsv: Boolean = corpus == Corpus.Tsv

          library match
            case Library.Caesura =>
              if tsv then '{ caesura.Benchmarks.caesuraParseTsv($document) }
              else '{ caesura.Benchmarks.caesuraParseCsv($document) }

            case Library.Univocity =>
              if tsv then '{ caesura.Benchmarks.univocityParseTsv($document.s) }
              else '{ caesura.Benchmarks.univocityParseCsv($document.s) }

            case Library.OpenCsv =>
              if tsv then '{ caesura.Benchmarks.openCsvParseTsv($document.s) }
              else '{ caesura.Benchmarks.openCsvParseCsv($document.s) }

            case Library.CommonsCsv =>
              if tsv then '{ caesura.Benchmarks.commonsParseTsv($document.s) }
              else '{ caesura.Benchmarks.commonsParseCsv($document.s) }

            case Library.FastCsv =>
              if tsv then '{ caesura.Benchmarks.fastCsvParseTsv($document.s) }
              else '{ caesura.Benchmarks.fastCsvParseCsv($document.s) }
