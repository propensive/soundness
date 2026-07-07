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

import anticipation.*
import contingency.*
import gossamer.*

// The Indian national calendar (reformed Saka), tied to the Gregorian calendar: a Saka year is
// `Gregorian − 78`, beginning on 1 Chaitra = 22 March (21 March when the Gregorian year is a leap
// year). Chaitra then has 30 days (31 in a leap year), the next five months 31, and the last six
// 30. Adopted by India in 1957 (1 Chaitra 1879 Saka = 22 March 1957).
class IndianCalendar() extends Calendar:
  type Mensual = IndianMonth
  type MonthUnit = IndianMonth.type

  val name: Text = t"Indian National"
  def monthsInYear(year: Year): Int = 12
  def monthOrdinal(year: Year, month: IndianMonth): Int = month.ordinal
  def monthOfOrdinal(year: Year, ordinal: Int): IndianMonth = IndianMonth.fromOrdinal(ordinal)

  private def gregorianLeap(gregorian: Int): Boolean =
    gregorian%4 == 0 && gregorian%100 != 0 || gregorian%400 == 0

  def leapYear(year: Year): Boolean = gregorianLeap(year() + 78)
  def daysInYear(year: Year): Int = if leapYear(year) then 366 else 365

  def daysInMonth(month: IndianMonth, year: Year): Int =
    if month.ordinal == 0 then (if leapYear(year) then 31 else 30)
    else if month.ordinal <= 5 then 31 else 30

  // The JDN of 1 Chaitra of a Saka year (its Gregorian-year start, 21 or 22 March).
  private def yearStart(saka: Int): Int =
    val gregorian = saka + 78
    val day = if gregorianLeap(gregorian) then 21 else 22
    unsafely(calendars.gregorianCalendar.jdn(Year(gregorian), Mar, Day(day))).jdn

  private def daysBeforeMonth(ordinal: Int, leap: Boolean): Int =
    if ordinal == 0 then 0 else
      val chaitra = if leap then 31 else 30
      chaitra + (if ordinal <= 6 then (ordinal - 1)*31 else 155 + (ordinal - 6)*30)

  def zerothDayOfYear(year: Year): Date = Date.julianDay(yearStart(year()) - 1)

  def annual(date: Date): Year =
    val gregorian = calendars.gregorianCalendar.annual(date)()
    Year(if date.jdn >= yearStart(gregorian - 78) then gregorian - 78 else gregorian - 79)

  def mensual(date: Date): IndianMonth =
    val saka = annual(date)()
    val doy = date.jdn - yearStart(saka)
    val chaitra = if leapYear(Year(saka)) then 31 else 30

    val ordinal =
      if doy < chaitra then 0 else
        val rest = doy - chaitra
        if rest < 155 then rest/31 + 1 else (rest - 155)/30 + 6

    IndianMonth.fromOrdinal(ordinal)

  def diurnal(date: Date): Day =
    val saka = annual(date)()
    val before = daysBeforeMonth(mensual(date).ordinal, leapYear(Year(saka)))
    Day(date.jdn - yearStart(saka) - before + 1)

  def jdn(year: Year, month: IndianMonth, day: Day): Date raises TimeError =
    if day() < 1 || day() > daysInMonth(month, year) then
      raise(TimeError(_.Invalid(year(), month.ordinal + 1, day(), this)))

    Date.julianDay(yearStart(year()) + daysBeforeMonth(month.ordinal, leapYear(year)) + day() - 1)
