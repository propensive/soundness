                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                    ╭───╮                                         ┃
┃  ╭─────────╮                                       │   │                                         ┃
┃  │   ╭─────╯╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮  ┃
┃  │   ╰─────╮│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯  ┃
┃  ╰─────╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ├╌╯╌─╯╰─╌ ╰───╮╰─╌ ╰───╮  ┃
┃  ╭─────╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╌   │╭───╌   │  ┃
┃  ╰─────────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯  ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0. © Copyright 2023-25 Jon Pretty, Propensive OÜ.                     ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        http://www.apache.org/licenses/LICENSE-2.0                                                ┃
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
import vacuous.*

abstract class RomanCalendar() extends Calendar:
  type Year = Int
  type Month = MonthName
  type Day = Int

  def leapYear(year: Year): Boolean

  def daysInMonth(month: Month, year: Year): Int = month match
    case Jan | Mar | May | Jul | Aug | Oct | Dec => 31
    case Apr | Jun | Sep | Nov                   => 30
    case Feb                                     => if leapYear(year) then 29 else 28

  def add(date: Date, period: Timespan): Date =
    val monthTotal = getMonth(date).ordinal + period.months
    val month2 = MonthName.fromOrdinal(monthTotal%12)
    val year2 = getYear(date) + period.years + monthTotal/12

    safely(julianDay(year2, month2, getDay(date)).addDays(period.days)).vouch

  def leapYearsSinceEpoch(year: Int): Int
  def daysInYear(year: Year): Int = if leapYear(year) then 366 else 365
  def zerothDayOfYear(year: Year): Date = Date.of(year*365 + leapYearsSinceEpoch(year) + 1721059)

  def getYear(date: Date): Int =
    def recur(year: Int): Int =
      val z = zerothDayOfYear(year).julianDay
      if z < date.julianDay && z + daysInYear(year) > date.julianDay then year else recur(year + 1)

    recur(((date.julianDay - 1721059)/366).toInt)

  def getMonth(date: Date): MonthName =
    val year = getYear(date)
    val ly = leapYear(year)
    MonthName.values.takeWhile(_.offset(ly) < date.yearDay(using this)).last

  def getDay(date: Date): Int =
    val year = getYear(date)
    val month = getMonth(date)
    date.julianDay - zerothDayOfYear(year).julianDay - month.offset(leapYear(year))

  def julianDay(year: Int, month: MonthName, day: Int): Date raises DateError =
    if day < 1 || day > daysInMonth(month, year)
    then raise
     (DateError(t"$year-${month.numerical}-$day"),
      Date(using calendars.julian)(2000, MonthName(1), 1))

    zerothDayOfYear(year).addDays(month.offset(leapYear(year)) + day)
