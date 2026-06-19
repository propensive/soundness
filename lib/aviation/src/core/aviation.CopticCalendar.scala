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
import gossamer.*

// The Coptic (Alexandrian) calendar: thirteen months — twelve of 30 days plus a short month of 5
// days (6 in a leap year) — with a leap year every fourth year (those with `year % 4 == 3`). Its
// epoch, 1 Thout 1 AM, is JDN 1825030 (29 August 284 CE in the Julian calendar). Dates are stored
// as the same Julian-day-number as every other calendar; only the labelling differs.
class CopticCalendar() extends Calendar:
  type Mensual = CopticMonth
  type MonthUnit = CopticMonth.type

  private val epoch: Int = 1825030 // JDN of 1 Thout 1 AM (29 August 284 CE, Julian)
  val name: Text = t"Coptic"
  def monthsInYear: Int = 13
  def monthOrdinal(month: CopticMonth): Int = month.ordinal
  def monthOfOrdinal(ordinal: Int): CopticMonth = CopticMonth.fromOrdinal(ordinal)

  def leapYear(year: Year): Boolean = year()%4 == 3
  def daysInYear(year: Year): Int = if leapYear(year) then 366 else 365

  def daysInMonth(month: CopticMonth, year: Year): Int =
    if month.ordinal < 12 then 30 else if leapYear(year) then 6 else 5

  def zerothDayOfYear(year: Year): Date =
    Date.julianDay(epoch - 1 + 365*(year() - 1) + year()/4)

  def annual(date: Date): Year = Year((4*(date.jdn - epoch) + 1463)/1461)

  private def dayOfYear(date: Date): Int = date.jdn - zerothDayOfYear(annual(date)).jdn

  def mensual(date: Date): CopticMonth =
    val doy = dayOfYear(date)
    CopticMonth.fromOrdinal(if doy > 360 then 12 else (doy - 1)/30)

  def diurnal(date: Date): Day =
    val doy = dayOfYear(date)
    Day(doy - 30*(if doy > 360 then 12 else (doy - 1)/30))

  def jdn(year: Year, month: CopticMonth, day: Day): Date raises TimeError =
    if day() < 1 || day() > daysInMonth(month, year) then
      raise(TimeError(_.Invalid(year(), month.ordinal + 1, day(), this)))

    Date.julianDay(epoch - 1 + 365*(year() - 1) + year()/4 + 30*month.ordinal + day())
