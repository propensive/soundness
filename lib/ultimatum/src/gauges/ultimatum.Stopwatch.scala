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
import aviation.*
import escapade.*
import gossamer.*
import iridescence.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*

object Stopwatch:
  // `hh:mm:ss`, dropping the hours until there are some. Padded, so the field does not change width
  // from one second to the next.
  private def digital(seconds: Long): Text =
    val total = seconds.max(0L)
    val minutes = total/60
    val hours = minutes/60

    def pad(value: Long): Text = if value < 10 then t"0$value" else value.show

    if hours > 0 then t"$hours:${pad(minutes%60)}:${pad(total%60)}"
    else t"${pad(minutes)}:${pad(total%60)}"

// How a duration is written. `Compact` gives the two largest units that carry information
// (`2m41s`); `Digital` gives a clock face (`02:41`), which is wider but does not change shape as it
// crosses a minute.
// The same designs serve `Elapsed` and `Countdown`, but through separate givens: the two are
// distinct types precisely so that a duration counting down can redden as it runs out while one
// counting up stays quiet.
enum Stopwatch:
  case Compact, Digital

  def write(seconds: Double): Text = this match
    case Compact => Magnitude.interval(seconds)
    case Digital => Stopwatch.digital(seconds.toLong)

  def columns(seconds: Double): Int = write(seconds).length

  // Elapsed time is reference material, not a warning, so it is drawn in the muted role.
  // Keyed on `aviation.Duration` itself: elapsed time is a duration, and wrapping it bought
  // nothing. `Countdown` keeps its own type, because the two must be able to appear together.
  def elapsed(using gauging: Gauging): Duration is Gaugeable = new Gaugeable:
    type Self = Duration
    override def elastic: Boolean = false
    override def minWidth(status: Duration): Int = 1
    override def columns(status: Duration): Int = Stopwatch.this.columns(status.value)

    def rows(status: Duration, tick: Tick, width: Int): List[Teletype] =
      List(draw(status.value, gauging.palette.muted, width, gauging))

  // A countdown, optionally colouring by how little is left: `urgent` reads the severity ramp
  // backwards, so the figure passes through the warning colour and reddens as it approaches zero.
  def countdown(urgent: Boolean)(using gauging: Gauging): Countdown is Gaugeable = new Gaugeable:
    type Self = Countdown
    override def elastic: Boolean = false
    override def minWidth(status: Countdown): Int = 1
    override def columns(status: Countdown): Int = Stopwatch.this.columns(status.duration.value)

    def rows(status: Countdown, tick: Tick, width: Int): List[Teletype] =
      val seconds = status.duration.value

      // Under a minute is where a countdown starts to matter; the ramp is read over that last
      // minute, so anything longer sits at the calm end of it.
      val color =
        if !urgent then gauging.palette.caption
        else gauging.palette.severity(1.0 - (seconds/60.0).min(1.0))

      List(draw(seconds, color, width, gauging))

  private def draw(seconds: Double, color: Color in Srgb, width: Int, gauging: Gauging)
  :   Teletype =

    val text = write(seconds)
    val used = gauging.cells(text)

    // Too narrow for the figure: drop its leading characters, so the seconds — the part that is
    // actually moving — are what survives.
    if used > width then gauging.tint(color)(Teletype(text.skip(used - width)))
    else gauging.tint(color)(Teletype(t"$text${t" "*(width - used)}"))
