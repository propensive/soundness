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
import gossamer.*
import hieroglyph.*
import spectacular.*
import symbolism.*

// Numeric formatting shared by both report renderers: one implementation decides digits
// and units; the renderers decide only colour and styling.
private[probably] object Format:
  val metrics = textMetrics.wideCharacterWidthMetric

  given measurable: Char is Measurable:
    def width(char: Char): Int = char match
      case '✓' | '✗' | '⎇' | '↑' | '↓' => 1
      case _                           => metrics.width(char)

  // A scaled quantity: `unit` counts how many divisions by 1000 were applied (0-2 selects
  // µs/ms/s or kB/MB/GB); 3 means the value overflowed the unit sequence and `whole` holds
  // the raw number with no fraction.
  case class Figure(whole: Text, fraction: Text, unit: Int)

  def scaled(n: Long, unit: Int = 0): Figure =
    if unit >= 3 then Figure(n.show, t"", 3)
    else if n > 100000L then scaled(n/1000L, unit + 1)
    else Figure((n/1000L).show, (n%1000L).show.pad(3, Rtl, '0'), unit)

  val timeUnits: List[Text] = List(t"µs", t"ms", t"s ")
  val memoryUnits: List[Text] = List(t"kB", t"MB", t"GB")

  // Basis points of `part` within `whole`, for percentages rendered to two decimal places.
  def basisPoints(part: Double, whole: Double): Long =
    if whole == 0.0 then 0L else (part*10000.0/whole).toLong

  def percent(basisPoints: Long): Text =
    t"${basisPoints/100}.${(basisPoints%100).show.pad(2, Rtl, '0')}"

  // A histogram bar of `samples` scaled against `max` over a 40-cell span: full blocks
  // with a final fractional character from the eighth-block series. Any nonzero count
  // shows at least the thinnest bar.
  def bar(samples: Long, max: Long): Text =
    val eighths = (if max == 0L then 0L else samples*320L/max).max(if samples > 0L then 1L else 0L)
    val partial = List(t"", t"▏", t"▎", t"▍", t"▌", t"▋", t"▊", t"▉")
    t"█"*(eighths/8L).toInt + partial((eighths%8L).toInt)

  val sparkBlocks: List[Text] = List(t"▁", t"▂", t"▃", t"▄", t"▅", t"▆", t"▇", t"█")

  def truncate(text: Text, max: Int = 800): Text =
    if text.length <= max then text else t"${text.keep(max)}…"

  def statusWord(status: Report.Status): Text = status match
    case Report.Status.Pass        => t"pass"
    case Report.Status.Fail        => t"fail"
    case Report.Status.Throws      => t"throws"
    case Report.Status.CheckThrows => t"check-throws"
    case Report.Status.Mixed       => t"mixed"
    case Report.Status.Suite       => t"suite"
    case Report.Status.Bench       => t"bench"
    case Report.Status.Stress      => t"stress"
    case Report.Status.Profile     => t"profile"
    case Report.Status.AspirePass  => t"aspire-pass"
    case Report.Status.AspireFail  => t"aspire-fail"
