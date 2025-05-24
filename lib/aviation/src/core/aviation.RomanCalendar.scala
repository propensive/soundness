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
┃    Soundness, version 0.31.0 for Scala 3.7.                                                      ┃
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
import contextual.*
import contingency.*
import fulminate.*
import gossamer.*
import spectacular.*
import symbolism.*
import vacuous.*

abstract class RomanCalendar() extends Calendar:
  type Annual = Year
  type Mensual = Month
  type Diurnal = Int

  def leapYear(year: Annual): Boolean

  def daysInMonth(month: Mensual, year: Annual): Int = month match
    case Jan | Mar | May | Jul | Aug | Oct | Dec => 31
    case Apr | Jun | Sep | Nov                   => 30
    case Feb                                     => if leapYear(year) then 29 else 28

  def add(date: Date, period: Timespan): Date =
    val monthTotal = mensual(date).ordinal + period.months
    val month2 = Month.fromOrdinal(monthTotal%12)
    val year2: Year = Year(annual(date).int + period.years + monthTotal/12)

    safely(julianDay(year2, month2, diurnal(date)).addDays(period.days)).vouch

  def leapYearsSinceEpoch(year: Year): Int
  def daysInYear(year: Annual): Int = if leapYear(year) then 366 else 365

  def zerothDayOfYear(year: Annual): Date =
    Date.of(year.int*365 + leapYearsSinceEpoch(year) + 1721059)

  def annual(date: Date): Year =
    def recur(year: Year): Year =
      val z = zerothDayOfYear(year).julianDay

      if z < date.julianDay && z + daysInYear(year) > date.julianDay then year
      else recur(year + 1)

    recur(Year(((date.julianDay - 1721059)/366).toInt))

  def mensual(date: Date): Month =
    val year = annual(date)
    val ly = leapYear(year)
    Month.values.takeWhile(_.offset(ly) < date.yearDay(using this)).last

  def diurnal(date: Date): Int =
    val year = annual(date)
    val month = mensual(date)
    date.julianDay - zerothDayOfYear(year).julianDay - month.offset(leapYear(year))

  def julianDay(year: Year, month: Month, day: Int): Date raises DateError =
    if day < 1 || day > daysInMonth(month, year) then
      raise(DateError(t"$year-${month.numerical}-$day"))
      Date(using calendars.julian)(Year(2000), Month(1), 1)

    zerothDayOfYear(year).addDays(month.offset(leapYear(year)) + day)
