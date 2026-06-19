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
import spectacular.*

// The tabular (arithmetical) Islamic (Hijri) calendar: twelve months alternating 30 and 29 days,
// with a 30th day added to the final month in the eleven leap years of each 30-year cycle (the
// "Kuwaiti" leap rule). Its civil epoch, 1 Muharram 1 AH, is JDN 1948440 (16 July 622 CE, Julian).
// Dates are the same Julian-day-number as in every other calendar; only the labelling differs.
class IslamicCalendar() extends Calendar:
  type Mensual = IslamicMonth
  type MonthUnit = IslamicMonth.type

  private val epoch: Int = 1948440 // JDN of 1 Muharram 1 AH (civil)
  val name: Text = t"Islamic"
  def monthsInYear(year: Year): Int = 12
  def monthOrdinal(year: Year, month: IslamicMonth): Int = month.ordinal
  def monthOfOrdinal(year: Year, ordinal: Int): IslamicMonth = IslamicMonth.fromOrdinal(ordinal)
  override def monthName(month: IslamicMonth): Text = month.show

  def leapYear(year: Year): Boolean = (11*year() + 14)%30 < 11
  def daysInYear(year: Year): Int = if leapYear(year) then 355 else 354

  def daysInMonth(month: IslamicMonth, year: Year): Int =
    if month.ordinal == 11 then (if leapYear(year) then 30 else 29)
    else if month.ordinal%2 == 0 then 30 else 29

  private def daysBeforeYear(year: Int): Int = 354*(year - 1) + (3 + 11*year)/30
  private def daysBeforeMonth(ordinal: Int): Int = (59*ordinal + 1)/2

  def zerothDayOfYear(year: Year): Date = Date.julianDay(epoch + daysBeforeYear(year()) - 1)

  def annual(date: Date): Year = Year((30*(date.jdn - epoch) + 10646)/10631)

  private def dayOfYear(date: Date): Int = date.jdn - epoch - daysBeforeYear(annual(date)())

  def mensual(date: Date): IslamicMonth =
    IslamicMonth.fromOrdinal((dayOfYear(date)*2/59).min(11))

  def diurnal(date: Date): Day =
    val doy = dayOfYear(date)
    Day(doy - daysBeforeMonth((doy*2/59).min(11)) + 1)

  def jdn(year: Year, month: IslamicMonth, day: Day): Date raises TimeError =
    if day() < 1 || day() > daysInMonth(month, year) then
      raise(TimeError(_.Invalid(year(), month.ordinal + 1, day(), this)))

    Date.julianDay(epoch + daysBeforeYear(year()) + daysBeforeMonth(month.ordinal) + day() - 1)
