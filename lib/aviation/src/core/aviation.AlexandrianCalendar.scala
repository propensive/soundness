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
package aviation

import contingency.*

// The shared structure of the "wandering"/Alexandrian calendars — Coptic, Ethiopian and the French
// Republican calendar: twelve months of 30 days followed by a short thirteenth month of 5 days (6
// in a leap year), with a leap year every fourth year (those with `year % 4 == 3`). Subclasses
// supply only the epoch (the JDN of their year 1, month 1, day 1), the name, and the month type.
trait AlexandrianCalendar extends Calendar:
  def epoch: Int

  def monthsInYear(year: Year): Int = 13
  def leapYear(year: Year): Boolean = year()%4 == 3
  def daysInYear(year: Year): Int = if leapYear(year) then 366 else 365

  def daysInMonth(month: Mensual, year: Year): Int =
    if monthOrdinal(year, month) < 12 then 30 else if leapYear(year) then 6 else 5

  def zerothDayOfYear(year: Year): Date =
    Date.julianDay(epoch - 1 + 365*(year() - 1) + year()/4)

  def annual(date: Date): Year = Year((4*(date.jdn - epoch) + 1463)/1461)

  private def dayOfYear(date: Date): Int = date.jdn - zerothDayOfYear(annual(date)).jdn

  def mensual(date: Date): Mensual =
    val doy = dayOfYear(date)
    monthOfOrdinal(annual(date), if doy > 360 then 12 else (doy - 1)/30)

  def diurnal(date: Date): Day =
    val doy = dayOfYear(date)
    Day(doy - 30*(if doy > 360 then 12 else (doy - 1)/30))

  def computeJdn(year: Year, month: Mensual, day: Day): Date =
    Date.julianDay(epoch - 1 + 365*(year() - 1) + year()/4 + 30*monthOrdinal(year, month) + day())
