/*
    Aviation, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package aviation

import anticipation.*
import fulminate.*
import rudiments.*

export Timing.{Instant, Duration}
export Aviation.Date
export MonthName.{Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec}

given realm: Realm = realm"aviation"

package calendars:
  given RomanCalendar as julian:
    def leapYear(year: Year): Boolean = year%4 == 0
    def leapYearsSinceEpoch(year: Int): Int = year/4

  given RomanCalendar as gregorian:
    def leapYear(year: Year): Boolean = year%4 == 0 && year%100 != 0 || year%400 == 0
    def leapYearsSinceEpoch(year: Int): Int = year/4 - year/100 + year/400 + 1

def now()(using clock: Clock): Instant = clock()
def today()(using clock: Clock, calendar: RomanCalendar, timezone: Timezone): Date = (now() in timezone).date

enum TimeEvent:
  case ParseStart

given (using Unapply[Text, Int]): Unapply[Text, Base60] =
  case As[Int](value: Base60) => Some(value)
  case _                      => None

given (using Unapply[Text, Int]): Unapply[Text, Base24] =
  case As[Int](value: Base24) => Some(value)
  case _                      => None

extension (inline double: Double)
  inline def am: Clockface = ${Aviation.validTime('double, false)}
  inline def pm: Clockface = ${Aviation.validTime('double, true)}

extension (one: 1)
  def year: Timespan = Timespan(StandardTime.Year, 1)
  def month: Timespan = Timespan(StandardTime.Month, 1)
  def week: Timespan = Timespan(StandardTime.Week, 1)
  def day: Timespan = Timespan(StandardTime.Day, 1)
  def hour: Timespan & FixedDuration = Timespan.fixed(StandardTime.Hour, 1)
  def minute: Timespan & FixedDuration = Timespan.fixed(StandardTime.Minute, 1)
  def second: Timespan & FixedDuration = Timespan.fixed(StandardTime.Second, 1)

extension (int: Int)
  def years: Timespan = Timespan(StandardTime.Year, int)
  def months: Timespan = Timespan(StandardTime.Month, int)
  def weeks: Timespan = Timespan(StandardTime.Week, int)
  def days: Timespan = Timespan(StandardTime.Day, int)
  def hours: Timespan & FixedDuration = Timespan.fixed(StandardTime.Hour, int)
  def minutes: Timespan & FixedDuration = Timespan.fixed(StandardTime.Minute, int)
  def seconds: Timespan & FixedDuration = Timespan.fixed(StandardTime.Second, int)

extension (inline context: StringContext)
  inline def tz(): Timezone = ${Timezone.Tz.expand('context)}
