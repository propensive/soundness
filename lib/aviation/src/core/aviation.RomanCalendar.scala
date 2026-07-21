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

import anticipation.*
import contingency.*
import symbolism.*

abstract class RomanCalendar(val name: Text) extends Calendar:
  type Mensual = Month
  type MonthUnit = Month.type

  def monthsInYear(year: Year): Int = 12
  def monthOrdinal(year: Year, month: Month): Int = month.ordinal
  def monthOfOrdinal(year: Year, ordinal: Int): Month = Month.fromOrdinal(ordinal)

  def leapYear(year: Year): Boolean

  def daysInMonth(month: Month, year: Year): Int = month match
    case Jan | Mar | May | Jul | Aug | Oct | Dec => 31
    case Apr | Jun | Sep | Nov                   => 30
    case Feb                                     => if leapYear(year) then 29 else 28

  def leapYearsSinceEpoch(year: Year): Int
  def daysInYear(year: Annual): Int = if leapYear(year) then 366 else 365

  def zerothDayOfYear(year: Annual): Date =
    Date.julianDay(year()*365 + leapYearsSinceEpoch(year - 1) + 1721059)

  def annual(date: Date): Year =
    val j = date.jdn + 32044
    val g = j/146097
    val dg = j%146097
    val c = ((dg/36524 + 1)*3)/4
    val dc = dg - c*36524
    val b = dc/1461
    val db = dc%1461
    val a = ((db/365 + 1)*3)/4
    val da = db - a * 365
    val y = g*400 + c*100 + b*4 + a
    val m = (da*5 + 308)/153 - 2
    val d = da - (m + 4)*153/5 + 122

    Year(y - 4800 + (m + 2)/12)

  def mensual(date: Date): Month =
    val j = date.jdn + 32044
    val g = j/146097
    val dg = j%146097
    val c = ((dg/36524 + 1)*3)/4
    val dc = dg - c*36524
    val db = dc%1461
    val a = ((db/365 + 1)*3)/4
    val da = db - a * 365
    val m = (da*5 + 308)/153 - 2

    Month.fromOrdinal((m + 2)%12)

  def diurnal(date: Date): Day =
    val j = date.jdn + 32044
    val g = j/146097
    val dg = j%146097
    val c = ((dg/36524 + 1)*3)/4
    val dc = dg - c*36524
    val db = dc%1461
    val a = ((db/365 + 1)*3)/4
    val da = db - a * 365
    val m = (da*5 + 308)/153 - 2

    Day(da - (m + 4)*153/5 + 122 + 1)

  def computeJdn(year: Year, month: Month, day: Day): Date =
    zerothDayOfYear(year).addDays(month.offset(leapYear(year)) + day())
