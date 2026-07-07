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
import spectacular.*

import HebrewMonth.*

// The Hebrew calendar: a lunisolar calendar of twelve months (thirteen in a leap year, when Adar I
// is inserted before Adar II), where the start of each year (1 Tishrei / Rosh Hashanah) is fixed by
// the *molad* (the mean lunar conjunction), adjusted by four postponement rules (*dechiyot*). Years
// run to 353/354/355 days (383/384/385 when leap); Cheshvan and Kislev absorb the variation. Leap
// years follow the 19-year Metonic cycle. Epoch: 1 Tishrei AM 1 = JDN 347998.
class HebrewCalendar() extends Calendar:
  type Mensual = HebrewMonth
  type MonthUnit = HebrewMonth.type

  private val epoch: Int = 347998
  val name: Text = t"Hebrew"

  override def monthName(month: HebrewMonth): Text = month.show

  def leapYear(year: Year): Boolean = (7*year() + 1)%19 < 7
  def monthsInYear(year: Year): Int = if leapYear(year) then 13 else 12

  // The civil-year position (0-based) of a month, accounting for the absence of Adar II in a common
  // year (which shifts Nisan onwards down by one), and the inverse.
  def monthOrdinal(year: Year, month: HebrewMonth): Int =
    if leapYear(year) || month.ordinal <= Adar.ordinal then month.ordinal else month.ordinal - 1

  def monthOfOrdinal(year: Year, ordinal: Int): HebrewMonth =
    if leapYear(year) || ordinal <= Adar.ordinal then HebrewMonth.fromOrdinal(ordinal)
    else HebrewMonth.fromOrdinal(ordinal + 1)

  // Days from the epoch to 1 Tishrei of `year`, with the ADU/molad-zaken postponement folded in.
  private def elapsedDays(year: Int): Int =
    val monthsElapsed = (235*year - 234)/19
    val partsElapsed = 12084 + 13753*monthsElapsed
    val days = 29*monthsElapsed + partsElapsed/25920
    if (3*(days + 1))%7 < 3 then days + 1 else days

  // The two remaining dechiyot (GaTaRaD and BeTUTeKaPoT), expressed as a year-length correction.
  private def newYearDelay(year: Int): Int =
    val prior = elapsedDays(year - 1)
    val current = elapsedDays(year)
    val next = elapsedDays(year + 1)
    if next - current == 356 then 2 else if current - prior == 382 then 1 else 0

  private def roshHashanah(year: Int): Int = epoch + elapsedDays(year) + newYearDelay(year)

  def daysInYear(year: Year): Int = roshHashanah(year() + 1) - roshHashanah(year())

  def daysInMonth(month: HebrewMonth, year: Year): Int = month match
    case Tishrei | Shevat | Nisan | Sivan | Av    => 30
    case Tevet | AdarSheni | Iyar | Tammuz | Elul => 29
    case Cheshvan                                 => if daysInYear(year)%10 == 5 then 30 else 29
    case Kislev                                   => if daysInYear(year)%10 == 3 then 29 else 30
    case Adar                                     => if leapYear(year) then 30 else 29

  def zerothDayOfYear(year: Year): Date = Date.julianDay(roshHashanah(year()) - 1)

  def annual(date: Date): Year =
    var year = (date.jdn - epoch)/365 + 1
    while roshHashanah(year + 1) <= date.jdn do year += 1
    while roshHashanah(year) > date.jdn do year -= 1
    Year(year)

  def mensual(date: Date): HebrewMonth =
    val year = annual(date)
    var remaining = date.jdn - roshHashanah(year())
    var ordinal = 0

    while remaining >= daysInMonth(monthOfOrdinal(year, ordinal), year) do
      remaining -= daysInMonth(monthOfOrdinal(year, ordinal), year)
      ordinal += 1

    monthOfOrdinal(year, ordinal)

  def diurnal(date: Date): Day =
    val year = annual(date)
    var remaining = date.jdn - roshHashanah(year())
    var ordinal = 0

    while remaining >= daysInMonth(monthOfOrdinal(year, ordinal), year) do
      remaining -= daysInMonth(monthOfOrdinal(year, ordinal), year)
      ordinal += 1

    Day(remaining + 1)

  def jdn(year: Year, month: HebrewMonth, day: Day): Date raises TimeError =
    if day() < 1 || day() > daysInMonth(month, year) then
      raise(TimeError(_.Invalid(year(), monthOrdinal(year, month) + 1, day(), this)))

    var total = roshHashanah(year())
    var ordinal = 0
    val target = monthOrdinal(year, month)

    while ordinal < target do
      total += daysInMonth(monthOfOrdinal(year, ordinal), year)
      ordinal += 1

    Date.julianDay(total + day() - 1)
