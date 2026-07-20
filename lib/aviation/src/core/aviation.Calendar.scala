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
import beneficence.*
import contingency.*
import gossamer.*
import spectacular.*

trait Calendar extends Findable:
  // All calendars number years and days-of-month with integers; only the month type varies (the
  // Gregorian `Month` enum, `IslamicMonth`, `CopticMonth`, …). `MonthUnit` is that month's radix,
  // used to dispatch calendar arithmetic to the right calendar at compile time.
  type Annual = Year
  type Diurnal = Day
  type Mensual
  type MonthUnit <: MonthRadix

  def name: Text
  // Year-aware to accommodate lunisolar calendars (the Hebrew calendar has 12 or 13 months, and the
  // month↔ordinal mapping shifts between common and leap years); other calendars ignore the year.
  def monthsInYear(year: Year): Int
  def daysInYear(year: Year): Int
  def daysInMonth(month: Mensual, year: Year): Int
  def monthOrdinal(year: Year, month: Mensual): Int
  def monthOfOrdinal(year: Year, ordinal: Int): Mensual
  def annual(date: Date): Year
  def mensual(date: Date): Mensual
  def diurnal(date: Date): Day
  def zerothDayOfYear(year: Year): Date

  // The Julian day number for a *valid* year/month/day, without range checking. This is the
  // per-calendar arithmetic; validity is enforced separately by `jdn`.
  def computeJdn(year: Year, month: Mensual, day: Day): Date

  // Whether a year/month/day denotes a real date. The default range-checks the day-of-month;
  // calendars with more complex validity (e.g. the French Republican `Regime`) override it.
  def validDate(year: Year, month: Mensual, day: Day): Boolean =
    day() >= 1 && day() <= daysInMonth(month, year)

  // `inline` so the `raises TimeError` context resolves at the call site: a non-inline `raises`
  // method called inside a deferred block (e.g. a test body) boxes the tactic it summons with the
  // block's own capability, which capture checking then rejects.
  inline def jdn(year: Year, month: Mensual, day: Day): Date raises TimeError =
    if !validDate(year, month, day)
    then raise(TimeError(_.Invalid(year(), monthOrdinal(year, month) + 1, day(), this)))

    computeJdn(year, month, day)

  // The display name of a month. By default this is the month value's own name (the enum case name,
  // which is already the conventional name in most calendars); calendars override it where the name
  // needs polishing.
  def monthName(month: Mensual): Text = month.toString.tt

  // A date rendered in this calendar, as "day month-name year", e.g. "15 Ramadan 1445".
  def format(date: Date): Text = t"${diurnal(date)()} ${monthName(mensual(date))} ${annual(date)()}"
