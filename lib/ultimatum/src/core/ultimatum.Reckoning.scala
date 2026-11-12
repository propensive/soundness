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
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object Reckoning:
  // How a count is written. `Padded` right-aligns the numerator to the total's digit count so the
  // field does not jitter as it counts up — the difference between a figure that can be read while
  // it changes and one that cannot.
  enum Counter:
    case Plain, Padded, Words, Percentage, Scaled

    def write(reckoning: Reckoning): Text =
      val done = reckoning.done.show

      this match
        case Plain      => reckoning.total.lay(done): total => t"$done/$total"
        case Words      => reckoning.total.lay(done): total => t"$done of $total"
        case Percentage => reckoning.fraction.lay(t"  ?%")(Magnitude.percentage(_))

        case Scaled =>
          val scaled = Magnitude.count(reckoning.done)
          reckoning.total.lay(scaled): total => t"$scaled/${Magnitude.count(total)}"

        // The numerator is right-aligned to the total's digit count, so the field does not jitter
        // as it counts up.
        case Padded =>
          reckoning.total.lay(done): total =>
            t"${t" "*(total.show.length - done.length).max(0)}$done/$total"

    def gaugeable(using gauging: Gauging): Reckoning is Gaugeable = new Gaugeable:
      type Self = Reckoning
      override def elastic: Boolean = false
      override def minWidth(status: Reckoning): Int = 1
      override def columns(status: Reckoning): Int = gauging.cells(write(status))

      def rows(status: Reckoning, tick: Tick, width: Int): List[Teletype] =
        val text = write(status)
        val used = gauging.cells(text)
        val body = gauging.tint(gauging.palette.caption)(Teletype(text))

        // Too narrow to write the count: drop the leading characters rather than the trailing
        // ones, so the numerator (which is what changes) survives longest.
        val fitted =
          if used <= width then e"$body${t" "*(width - used)}"
          else gauging.tint(gauging.palette.caption)(Teletype(text.skip(used - width)))

        List(fitted)

  // The default: `17/120` is not a style question.
  given gaugeable: Gauging => Reckoning is Gaugeable = counters.plainCounter

// A count of work done against a total that may not be known yet: `17/120`, or just `17` while the
// total is still being discovered. The status a counter shows, and the one most jobs actually have
// to hand.
case class Reckoning(done: Long, total: Optional[Long] = Unset):
  def fraction: Optional[Fraction] = total.let: total =>
    if total <= 0 then Fraction(0.0) else Fraction(done.toDouble/total)

  def remaining: Optional[Long] = total.let: total => (total - done).max(0L)
