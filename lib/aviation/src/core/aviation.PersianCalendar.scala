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

import java.lang.Math.{floorDiv, floorMod}

import anticipation.*
import contingency.*
import gossamer.*

// The Persian (Solar Hijri) calendar, using the arithmetical 2820-year cycle (Birashk's algorithm)
// as a deterministic approximation of the official, observational calendar (true vernal equinox at
// the 52.5°E meridian); the two can differ by a day around some equinoxes. Months 1–6 have 31 days,
// 7–11 have 30, and Esfand has 29 (30 in a leap year). Epoch: 1 Farvardin 1 = JDN 1948321.
class PersianCalendar() extends Calendar:
  type Mensual = PersianMonth
  type MonthUnit = PersianMonth.type

  private val epoch: Int = 1948321
  val name: Text = t"Persian"
  def monthsInYear(year: Year): Int = 12
  def monthOrdinal(year: Year, month: PersianMonth): Int = month.ordinal
  def monthOfOrdinal(year: Year, ordinal: Int): PersianMonth = PersianMonth.fromOrdinal(ordinal)

  def leapYear(year: Year): Boolean = ((floorMod(year() - 474, 2820) + 512)*682)%2816 < 682
  def daysInYear(year: Year): Int = if leapYear(year) then 366 else 365

  def daysInMonth(month: PersianMonth, year: Year): Int =
    if month.ordinal < 6 then 31 else if month.ordinal < 11 then 30
    else if leapYear(year) then 30 else 29

  // The JDN of (year, monthOrdinal, day) by Birashk's arithmetic.
  private def toJulianDay(year: Int, monthOrdinal: Int, day: Int): Int =
    val base = year - (if year >= 0 then 474 else 473)
    val cyclic = 474 + floorMod(base, 2820)
    val before = if monthOrdinal <= 6 then monthOrdinal*31 else monthOrdinal*30 + 6
    val cyclicDays = (cyclic*682 - 110)/2816 + (cyclic - 1)*365
    val cycleDays = floorDiv(base, 2820)*1029983
    day + before + cyclicDays + cycleDays + epoch - 1

  def zerothDayOfYear(year: Year): Date = Date.julianDay(toJulianDay(year(), 0, 0))

  def annual(date: Date): Year =
    val depoch = date.jdn - toJulianDay(475, 0, 1)
    val cycle = floorDiv(depoch, 1029983)
    val yearInCycle = floorMod(depoch, 1029983)

    val offset =
      if yearInCycle == 1029982 then 2820 else
        val a = yearInCycle/366
        val b = yearInCycle%366
        (2134*a + 2816*b + 2815)/1028522 + a + 1

    Year(offset + 2820*cycle + 474)

  private def dayOfYear(date: Date): Int = date.jdn - toJulianDay(annual(date)(), 0, 1) + 1

  def mensual(date: Date): PersianMonth =
    val doy = dayOfYear(date)
    PersianMonth.fromOrdinal(if doy <= 186 then (doy - 1)/31 else (doy - 187)/30 + 6)

  def diurnal(date: Date): Day =
    Day(date.jdn - toJulianDay(annual(date)(), mensual(date).ordinal, 1) + 1)

  def computeJdn(year: Year, month: PersianMonth, day: Day): Date =
    Date.julianDay(toJulianDay(year(), month.ordinal, day()))
