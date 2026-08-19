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
package probably

import anticipation.*
import fulminate.*
import vacuous.*

// The renderer-agnostic report document: one structural representation of a test report,
// built by `Documenting` and rendered by `AnsiRenderer` (full colour) or `TerseRenderer`
// (plain text). Data is semantic, never preformatted: backends decide colour and glyphs,
// and `Format` decides digits, so the two output modes cannot drift structurally.
private[probably] object Doc:
  enum Datum:
    case Blank
    case Gap                                     // a sparse-grid hole: an omitted combination
    case Str(text: Text)
    case Hash(id: Text)
    case Title(name: Message, depth: Int)
    case Mark(status: Report.Status)             // a glyph in colour output, a word in terse
    case Num(value: Long)
    case Time(nanos: Long)
    case Memory(bytes: Long)
    case Rate(perSecond: Long)                   // operations per second
    case Percent(fraction: Double)
    case Conf(percentile: Int, basisPoints: Long)  // e.g. P95 ±2.10%
    case Ratio(factor: Double)                   // baseline-relative; 1.0 renders as ★
    case Delta(datum: Datum, negative: Boolean)  // arithmetic baseline-relative: signed

  case class Column(title: Text, numeric: Boolean = false)

  // One sequence of a sparkline panel: block levels (1-8) per step, with cells beyond the
  // sustained concurrency flagged for subdual, and the sustained (N, throughput) summary.
  case class Spark
    ( label:     Text,
      cells:     List[Optional[(Int, Boolean)]],
      sustained: Optional[(Long, Long)] )

  enum Block:
    // A table of cells; a biaxial entry renders as a crosstab: its second axis's values
    // become the columns and each cell holds only the headline datum.
    case Table(title: Optional[Test.Id], columns: List[Column], rows: List[List[Datum]])
    case Sparkline(steps: List[Long], sequence: List[Spark])
    case Histogram(title: Optional[Test.Id], total: Long, frames: List[Hotspots.Frame])

  // How much of a report to render. `Summary` shows only each group's headline blocks;
  // `Verbose` additionally shows its `detail` blocks, which carry the full per-coordinate
  // breakdown that each headline block condenses.
  enum Verbosity:
    case Summary, Verbose

    def verbose: Boolean = this == Verbosity.Verbose

  // A group of measurement blocks belonging to one suite, of one kind, rendered with a
  // ribbon header (and a GitHub Actions group when applicable). `blocks` are always
  // rendered; `detail` only under `Verbosity.Verbose`.
  case class Group
    ( suite:  Optional[Testable],
      kind:   Entry.Kind,
      blocks: List[Block],
      detail: List[Block] = Nil )

  // One row of the global results table, aggregating a test's runs across all its cells.
  case class SummaryRow
    ( status: Report.Status, id: Test.Id, count: Int, min: Long, max: Long, avg: Long )

  case class Totals(passed: Int, failed: Int, aspirePassed: Int, aspireFailed: Int):
    def total: Int = passed + failed + aspirePassed + aspireFailed

  case class Document
    ( results:   List[SummaryRow],
      totals:    Totals,
      groups:    List[Group],
      failures:  List[(Test.Id, List[Verdict.Detail])],
      fatal:     Optional[(Throwable, Set[Test.Id])],
      verbosity: Verbosity = Verbosity.Summary )
