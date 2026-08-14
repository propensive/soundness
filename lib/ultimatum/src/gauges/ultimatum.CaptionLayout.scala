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
package ultimatum

import anticipation.*
import escapade.*
import gossamer.*
import hieroglyph.*
import symbolism.*
import tessellate.*

object CaptionLayout:
  // The no-import default: the label follows the gauge, one space away.
  given default: CaptionLayout = CaptionLayout(1, true, true)

// Where a caption sits relative to the gauge it labels, and what happens when there is not room for
// both. `elide` shortens the caption rather than squeezing the gauge, which is the right priority:
// a half-drawn bar misreports the thing it is measuring, whereas a shortened label is merely
// terser.
case class CaptionLayout(gap: Int, trailing: Boolean, elide: Boolean):
  // How many cells the gauge itself gets. The caption is allowed at most half the row before it
  // starts costing the gauge cells — so a long label shortens rather than crowding out the thing it
  // labels, and a short one still leaves an elastic bar the rest of the row.
  def gaugeWidth(preferred: Int, caption: Text, width: Int, gauging: Gauging): Int =
    val wanted = gauging.cells(caption)
    val allowance = if elide then wanted.min(width/2) else wanted
    (width - gap - allowance).max(0).min(preferred)

  def compose(gauge: Teletype, gaugeWidth: Int, caption: Text, width: Int, gauging: Gauging)
  :   Teletype =

    val room = width - gaugeWidth - gap
    val label = if !elide then caption else shorten(caption, room, gauging)
    val text = gauging.tint(gauging.palette.caption)(Teletype(label))
    val spacer = t" "*gap.max(0)

    val composed =
      if room <= 0 then gauge else if trailing then e"$gauge$spacer$text" else e"$text$spacer$gauge"

    given Text is Measurable = gauging.metric
    Alignment.Left.pad(composed, width)

  private def shorten(caption: Text, room: Int, gauging: Gauging): Text =
    given Text is Measurable = gauging.metric
    if room <= 0 then t"" else Flow.shorten(caption, room)
