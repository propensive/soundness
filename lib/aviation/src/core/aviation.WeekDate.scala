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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import contingency.*

// ISO-8601 week dates label a `Date` not by month but by (week-year, week, weekday): weeks run
// Monday to Sunday, week 1 is the week containing 4 January (the first Thursday), and a date near
// the year boundary may belong to the previous or next week-year. This isn't a `Calendar` (it has
// no months); it's a labelling of a Gregorian date, so the relevant `RomanCalendar` is contextual.
object WeekDate:
  // ISO weekday number, Monday = 1 … Sunday = 7 (the `Weekday` enum runs Mon = 0).
  private def isoWeekday(date: Date): Int = date.weekday.ordinal + 1

  // The week number a date falls in, counted within its own (Gregorian) year — possibly < 1 or
  // greater than that year's week count, in which case it belongs to the adjacent week-year.
  private def rawWeek(date: Date)(using calendar: RomanCalendar): Int =
    val dayOfYear = date.jdn - calendar.zerothDayOfYear(calendar.annual(date)).jdn
    (dayOfYear - isoWeekday(date) + 10)/7

  // The number of ISO weeks in a week-year: 28 December always lies in the last week.
  private def weekCount(year: Year)(using RomanCalendar): Int =
    rawWeek(unsafely(Date(year, Dec, Day(28))))

  def weekYear(date: Date)(using calendar: RomanCalendar): Year =
    val week = rawWeek(date)
    val year = calendar.annual(date)
    if week < 1 then Year(year() - 1) else if week > weekCount(year) then Year(year() + 1) else year

  def weekOfYear(date: Date)(using calendar: RomanCalendar): Int =
    val week = rawWeek(date)
    val year = calendar.annual(date)
    if week < 1 then weekCount(Year(year() - 1)) else if week > weekCount(year) then 1 else week

  // The `Date` for a given ISO (week-year, week, weekday): start from the Monday of week 1 (the
  // week containing 4 January) and step forward.
  def apply(weekYear: Year, week: Int, weekday: Weekday)(using RomanCalendar)
  :   Date raises TimeError =

    val jan4 = Date(weekYear, Jan, Day(4))
    jan4.addDays(-jan4.weekday.ordinal + (week - 1)*7 + weekday.ordinal)
