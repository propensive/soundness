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
package tasseomancy

import anticipation.*
import contingency.*
import phoenicia.*
import quantitative.*
import vacuous.*

object FontMetric:
  // The no-import default follows the style's font: the real advances of an embedded font file,
  // so the face measured is the face rendered, or an average width per character for a font
  // whose file is not to hand, which lays out a chart acceptably in any proportional face. Any
  // `import fontMetrics.…` outranks this.
  given fromStyle: (style: Chart.Style) => FontMetric = FontMetric.of(style.font)

  // Widths summed from a font's own glyph advances, for the face the chart will be shown in;
  // margins and legends then fit their text exactly. A character the face lacks would raise, so
  // the average width stands in for any text containing one.
  def of(font: Sfnt): FontMetric = text =>
    safely(font.width(text)).or(tasseomancy.fontMetrics.averageFontMetric.width(text))

  // The metric of a font's first embedded file, if its provision has one, else the average.
  def of(font: Font): FontMetric = embedded(font.provision.sources) match
    case Unset      => tasseomancy.fontMetrics.averageFontMetric
    case sfnt: Sfnt => of(sfnt)

  private def embedded(sources: List[Typesettable.Source]): Optional[Sfnt] = sources match
    case Typesettable.Source.Embedded(sfnt) :: _ => sfnt
    case _ :: tail                               => embedded(tail)
    case _                                       => Unset

// The width of a run of text in ems, as the chart's typeface will set it. Multiplied by the
// style's font size, this is what decides how much room an axis's labels and a legend need.
trait FontMetric:
  def width(text: Text): Quantity[Ems[1]]
