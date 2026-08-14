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
import gossamer.*

// A calendar with no months: a date is a year plus a day-of-year (1–366) — the ISO-8601 "ordinal
// date" form, e.g. day 247 of 2024. Year boundaries are delegated to the Gregorian calendar; the
// single vestigial "month" (`Annus`) spans the whole year, so `diurnal` is the day-of-year.
object OrdinalCalendar extends Calendar:
  object Annus extends MonthRadix
  type Mensual = Annus.type
  type MonthUnit = Annus.type

  private def base: RomanCalendar = calendars.gregorianCalendar

  def name: Text = t"Ordinal"
  def monthsInYear(year: Year): Int = 1
  def daysInYear(year: Year): Int = base.daysInYear(year)
  def daysInMonth(month: Annus.type, year: Year): Int = base.daysInYear(year)
  def monthOrdinal(year: Year, month: Annus.type): Int = 0
  def monthOfOrdinal(year: Year, ordinal: Int): Annus.type = Annus
  def annual(date: Date): Year = base.annual(date)
  def mensual(date: Date): Annus.type = Annus
  def zerothDayOfYear(year: Year): Date = base.zerothDayOfYear(year)

  def diurnal(date: Date): Day = Day(date.jdn - base.zerothDayOfYear(base.annual(date)).jdn)

  def computeJdn(year: Year, month: Annus.type, day: Day): Date =
    base.zerothDayOfYear(year).addDays(day())

  // Construct directly from a year and a day-of-year, without the vestigial month.
  inline def apply(year: Year, dayOfYear: Int): Date raises Moment.Error = jdn(year, Annus, Day(dayOfYear))

  override def format(date: Date): Text = t"${annual(date)()}-${diurnal(date)()}"
