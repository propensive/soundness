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
package escritoire

import anticipation.*
import gossamer.*
import hieroglyph.*
import polysyllabic.*
import rudiments.*
import tessellate.*

// A column-sizing strategy. `flex` describes the column's claim on the table's width — its
// intrinsic bounds, its appetite for spare space, and whether it may collapse entirely when
// the table cannot fit — from the aggregate intrinsic widths of every logical line the column
// will display; `fit` then arranges one cell's lines into the width the table settled on.
//
// The claim is a function of the aggregate `Metrics` alone, which is the max-fold of each
// line's own: so a layout can carry the aggregate, admit a new row by folding its cells in,
// and know — by `accommodates` — when a row cannot move the column at all.
object Columnar:
  // The aggregate intrinsic widths of some lines: what every strategy's claim derives from.
  def metrics[text: Textual { type Result = Char }](lines: Array[text]^{})
    ( using Text is Measurable )
  :   Metrics =

    var metrics = Metrics(0, 0)
    lines.each { line => metrics = metrics.max(Flow.metrics(line)) }
    metrics

trait Columnar:
  def flex(metrics: Metrics, maxWidth: Int): Flex

  def flex[text: Textual { type Result = Char }](lines: Array[text]^{}, maxWidth: Int)
    ( using Text is Measurable )
  :   Flex =

    flex(Columnar.metrics(lines), maxWidth)

  // Whether a cell with intrinsic widths `cell` is already covered by the column's `aggregate`,
  // so admitting it leaves the claim, and hence the solved widths, unchanged.
  def accommodates(aggregate: Metrics, cell: Metrics): Boolean =
    cell.min <= aggregate.min && cell.natural <= aggregate.natural

  def fit[text: Textual { type Result = Char }]
    ( lines: Array[text]^{}, width: Int, textAlign: TextAlignment )
    ( using Text is Measurable, Hyphenation )
  :   Sequence[text]
