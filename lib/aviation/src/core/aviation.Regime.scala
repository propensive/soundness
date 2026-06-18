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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package aviation

import anticipation.*
import contingency.*
import fulminate.*
import vacuous.*

// A `Regime` is the calendar in force as it changed through history: an ordered sequence of
// segments, each handing over to the next at a Julian day number. Because the Julian day number is
// a calendar-independent axis, a date is located in whichever segment governs it, and translation
// across a boundary is automatic. The two-segment case is a Julian-to-Gregorian hybrid; the dates
// that exist in neither adjacent calendar (the cutover gap, e.g. 1582-10-05..14) are rejected.
object Regime:
  case class Segment(from: Date, calendar: RomanCalendar)

  def apply(name: Text, segments: Segment*): Regime =
    new Regime(name, segments.to(List).sortBy(_.from.jdn))

class Regime(name: Text, segments: List[Regime.Segment]) extends RomanCalendar(name):
  import Regime.Segment

  // Each segment paired with the inclusive upper Julian-day bound at which the next takes over. The
  // bound is the next segment's first day: the two calendars are aligned so that the last day of
  // one and the first day of the next share a Julian day number (Julian 1582-10-04 and Gregorian
  // 1582-10-15 are the same day), so that shared day is valid under both — the bound is inclusive.
  private val bounded: List[(Segment, Int)] =
    segments.zip(segments.drop(1).map(_.from.jdn) :+ Int.MaxValue)

  // The calendar governing a Julian day number: the latest segment to have taken effect by then.
  private def at(date: Date): RomanCalendar =
    bounded.filter(_(0).from.jdn <= date.jdn).lastOption.map(_(0).calendar).getOrElse:
      segments.head.calendar

  // The regime's Julian day number for a field date, or `Unset` if it falls in a gap between two
  // calendars (e.g. 1582-10-10 under the Papal cutover) or is otherwise an invalid date.
  private def locate(year: Year, month: Month, day: Day): Optional[Date] =
    def recur(remaining: List[(Segment, Int)]): Optional[Date] = remaining match
      case (segment, upper) :: tail =>
        val candidate: Optional[Date] = safely(segment.calendar.jdn(year, month, day))
        val jdn: Optional[Int] = candidate.let(_.jdn)

        if jdn.present && segment.from.jdn <= jdn.vouch && jdn.vouch <= upper
        then candidate
        else recur(tail)

      case Nil =>
        Unset

    recur(bounded)

  // The calendar governing a year, sampled at its midpoint (never inside a historical cutover gap).
  private def governing(year: Year): RomanCalendar =
    locate(year, Jun, Day(15)).let(at).or(segments.last.calendar)

  override def annual(date: Date): Year = at(date).annual(date)
  override def mensual(date: Date): Month = at(date).mensual(date)
  override def diurnal(date: Date): Day = at(date).diurnal(date)

  def leapYear(year: Year): Boolean = governing(year).leapYear(year)
  def leapYearsSinceEpoch(year: Year): Int = governing(year).leapYearsSinceEpoch(year)

  override def jdn(year: Year, month: Month, day: Day): Date raises TimeError =
    val located = locate(year, month, day)

    if located.present then located.vouch else
      raise(TimeError(_.Invalid(year(), month.numerical, day(), this)))
      Date.julianDay(0)
