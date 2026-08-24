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


import scala.collection.immutable.IndexedSeq

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import gossamer.Textual.concatenable
import hieroglyph.*
import polysyllabic.*
import rudiments.*
import symbolism.*
import tessellate.*
import vacuous.*

extension [row](data: List[row])
  def tabulation[text: Textual](using tabulable: row is Tabulable[text]): Tabulation[text] =

    tabulable.tabulate(data)

extension [value](value: value)
  def tabulation[text: Textual](using tabular: value is Tabular[text]): Tabulation[text] =
    tabular.tabulate(value)

// `failAttenuation` is a context function `Tactic[Table.Error] ?=> Attenuation^`: the returned
// `Attenuation` captures the Tactic *parameter*, so the given value itself captures nothing from its
// enclosing scope and these can stay package-level givens — accessing `failAttenuation` or
// `ignoreAttenuation` then captures no capability (unlike a member of an `ExclusiveCapability` object).
package columnAttenuation:
  given failAttenuation: (tactic: Tactic[Table.Error]) => (Attenuation^{tactic}) =
    (minimum, available) => raise(Table.Error(minimum, available))

  given ignoreAttenuation: Attenuation = (minimum, available) => ()

package tableStyles:
  import tessellate.BoxLine.*
  import tessellate.LineCharset.{Default, Rounded}

  given defaultTableStyle: TableStyle = TableStyle(1, Thick, Thick, Thin, Thick, Thin, Default)
  given thinRoundedTableStyle: TableStyle = TableStyle(1, Thin, Thin, Thin, Thin, Thin, Rounded)
  given horizontalTableStyle: TableStyle = TableStyle(1, Thin, Thin, Thin, Blank, Blank, Default)
  given midOnlyTableStyle: TableStyle = TableStyle(1, Blank, Blank, Thin, Blank, Blank, Default)
  given verticalTableStyle: TableStyle = TableStyle(1, Blank, Blank, Blank, Thin, Thin, Default)
  given minimalTableStyle: TableStyle = TableStyle(1, Unset, Unset, Thin, Blank, Blank, Default)

// The old table alignment vocabulary is now the shared `tessellate` one; the escritoire
// names remain as aliases.
type TextAlignment = tessellate.Alignment
val TextAlignment: tessellate.Alignment.type = tessellate.Alignment
type VerticalAlignment = tessellate.Alignment.Vertical
val VerticalAlignment: tessellate.Alignment.Vertical.type = tessellate.Alignment.Vertical

package columnar:
  private def columnMetrics[textual: Textual { type Result = Char }](lines: Array[textual]^{})
    ( using Text is Measurable )
  :   Metrics =

    var metrics = Metrics(0, 0)
    lines.each { line => metrics = metrics.max(Flow.metrics(line)) }
    metrics

  object Paragraph extends Columnar:
    def flex[textual: Textual { type Result = Char }](lines: Array[textual]^{}, maxWidth: Int)
      ( using Text is Measurable )
    :   Flex =

      Flex.content(columnMetrics(lines))


    def fit[textual: Textual { type Result = Char }]
      ( lines: Array[textual]^{}, width: Int, textAlign: TextAlignment )
      ( using Text is Measurable, Hyphenation )
    :   Sequence[textual] =


        lines.readable.to(IndexedSeq).bind(Flow.wrap(_, width).stdlib.to(List)).toVector
        . pipe(Sequence.from(_))
  object ParagraphOrBreak extends Columnar:
    // Elastic between a single cell and its natural width: the strategy prefers word
    // wrapping but will chop mid-word rather than overflow, so it has no min-content floor.
    def flex[textual: Textual { type Result = Char }](lines: Array[textual]^{}, maxWidth: Int)
      ( using Text is Measurable )
    :   Flex =

      val metrics = columnMetrics(lines)
      val floor = metrics.natural.min(1)
      Flex(Metrics(floor, metrics.natural), (metrics.natural - floor).max(0).toDouble, metrics.natural)


    def fit[textual: Textual { type Result = Char }]
      ( lines: Array[textual]^{}, width: Int, textAlign: TextAlignment )
      ( using Text is Measurable, Hyphenation )
    :   Sequence[textual] =

      if columnMetrics(lines).min < width then Paragraph.fit(lines, width, textAlign)
      else

          lines.readable.to(IndexedSeq).bind(Flow.chop(_, width).stdlib.to(List)).toVector
          . pipe(Sequence.from(_))
  case class Fixed(fixedWidth: Int, ellipsis: Text = t"…") extends Columnar:
    def flex[text: Textual { type Result = Char }](lines: Array[text]^{}, maxWidth: Int)
      ( using Text is Measurable )
    :   Flex =

      Flex(Metrics(fixedWidth), 0.0, fixedWidth)


    def fit[text: Textual { type Result = Char }]
      ( lines: Array[text]^{}, width: Int, textAlign: TextAlignment )
      ( using Text is Measurable, Hyphenation )
    :   Sequence[text] =

      Sequence.from(lines.readable.toVector.map(Flow.shorten(_, width, ellipsis)))

  case class Shortened(fixedWidth: Int, ellipsis: Text = t"…") extends Columnar:
    // Elastic between one cell and its natural width, truncating whatever exceeds the
    // settled width with an ellipsis.
    def flex[text: Textual { type Result = Char }](lines: Array[text]^{}, maxWidth: Int)
      ( using Text is Measurable )
    :   Flex =

      val natural = columnMetrics(lines).natural
      val floor = natural.min(1)
      Flex(Metrics(floor, natural), (natural - floor).max(0).toDouble, natural)


    def fit[text: Textual { type Result = Char }]
      ( lines: Array[text]^{}, width: Int, textAlign: TextAlignment )
      ( using Text is Measurable, Hyphenation )
    :   Sequence[text] =

      Sequence.from(lines.readable.toVector.map(Flow.shorten(_, width, ellipsis)))

  case class Collapsible(threshold: Double) extends Columnar:
    // Rigid at its natural width, but drops from the table entirely when space runs out;
    // a higher threshold collapses earlier (a lower rank collapses first).
    def flex[text: Textual { type Result = Char }](lines: Array[text]^{}, maxWidth: Int)
      ( using Text is Measurable )
    :   Flex =

      val natural = columnMetrics(lines).natural
      Flex(Metrics(natural), 0.0, rank = ((1.0 - threshold)*1000).toInt, collapsible = true)


    def fit[text: Textual { type Result = Char }]
      ( lines: Array[text]^{}, width: Int, textAlign: TextAlignment )
      ( using Text is Measurable, Hyphenation )
    :   Sequence[text] =

      Sequence.from(lines.readable.toVector)
