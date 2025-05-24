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
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import spectacular.*
import vacuous.*

export Aviation2.{Instant, Duration}
export Aviation.{Date, Year}
export Month.{Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec}

given realm: Realm = realm"aviation"

package dateFormats:
  private given calendar: RomanCalendar = calendars.gregorian

  given european: Date is Showable =
    import endianness.littleEndian, numerics.fixedWidth, separation.dot, yearFormats.full
    Date.showable.text(_)

  given american: Date is Showable =
    import endianness.middleEndian, numerics.fixedWidth, separation.slash, yearFormats.full
    Date.showable.text(_)

  given unitedKingdom: Date is Showable =
    import endianness.littleEndian, numerics.fixedWidth, separation.slash, yearFormats.full
    Date.showable.text(_)

  given southEastAsia: Date is Showable =
    import endianness.littleEndian, numerics.fixedWidth, separation.hyphen, yearFormats.full
    Date.showable.text(_)

  given iso8601: Date is Showable =
    import endianness.bigEndian, numerics.fixedWidth, separation.hyphen, yearFormats.full
    Date.showable.text(_)

  package endianness:
    given bigEndian: Endianness = Endianness.BigEndian
    given littleEndian: Endianness = Endianness.LittleEndian
    given middleEndian: Endianness = Endianness.MiddleEndian

  package numerics:
    given fixedWidth: DateNumerics = DateNumerics.FixedWidth
    given variableWidth: DateNumerics = DateNumerics.VariableWidth

  package separation:
    given slash: DateSeparation = () => t"/"
    given hyphen: DateSeparation = () => t"-"
    given dot: DateSeparation = () => t"."
    given space: DateSeparation = () => t" "

  package yearFormats:
    given twoDigits: YearFormat = YearFormat.TwoDigitYear
    given full: YearFormat = YearFormat.FullYear

package timeFormats:

  given military: Clockface is Showable =
    import hourCount.twentyFourHour, numerics.fixedWidth, separation.none, specificity.minutes
    Clockface.showable.text(_)

  given civilian: Clockface is Showable =
    import hourCount.twelveHour, meridiems.upper, numerics.fixedWidth, separation.colon
    import specificity.minutes

    Clockface.showable.text(_)

  given associatedPress: Clockface is Showable =
    import hourCount.twelveHour, meridiems.lowerPunctuated, numerics.variableWidth, separation.colon
    import specificity.minutes
    Clockface.showable.text(_)

  given french: Clockface is Showable =
    import hourCount.twentyFourHour, numerics.fixedWidth, separation.french, specificity.minutes
    Clockface.showable.text(_)

  given iso8601: Clockface is Showable =
    import hourCount.twentyFourHour, numerics.fixedWidth, separation.colon, specificity.seconds
    Clockface.showable.text(_)

  given ledger: Clockface is Showable =
    import hourCount.twentyFourHour, numerics.fixedWidth, separation.dot, specificity.minutes
    Clockface.showable.text(_)

  given railway: Clockface is Showable =
    import hourCount.twentyFourHour, numerics.fixedWidth, separation.colon, specificity.minutes
    Clockface.showable.text(_)

  package meridiems:
    given upper: Meridiem is Showable =
      case Meridiem.Am => t"AM"
      case Meridiem.Pm => t"PM"

    given lower: Meridiem is Showable =
      case Meridiem.Am => t"am"
      case Meridiem.Pm => t"pm"

    given upperPunctuated: Meridiem is Showable =
      case Meridiem.Am => t"A.M."
      case Meridiem.Pm => t"P.M."

    given lowerPunctuated: Meridiem is Showable =
      case Meridiem.Am => t"a.m."
      case Meridiem.Pm => t"p.m."

  package hourCount:
    given twelveHour: (Meridiem is Showable) => TimeFormat:
      def postfix(meridiem: Meridiem): Text = t" ${meridiem}"
      def halfDay: Boolean = true
      def seconds: Boolean = false

    given twelveHourSeconds: (Meridiem is Showable) => TimeFormat:
      def postfix(meridiem: Meridiem): Text = t" ${meridiem}"
      def halfDay: Boolean = true
      def seconds: Boolean = false

    given twentyFourHour: TimeFormat:
      def postfix(meridiem: Meridiem): Text = t""
      def halfDay: Boolean = false
      def seconds: Boolean = false

    given twentyFourHourSeconds: TimeFormat:
      def postfix(meridiem: Meridiem): Text = t""
      def halfDay: Boolean = false
      def seconds: Boolean = true

  package specificity:
    given minutes: TimeSpecificity = TimeSpecificity.Minutes
    given seconds: TimeSpecificity = TimeSpecificity.Seconds

  package numerics:
    given fixedWidth: TimeNumerics = TimeNumerics.FixedWidth
    given variableWidth: TimeNumerics = TimeNumerics.VariableWidth

  package separation:
    given dot: TimeSeparation = () => t"."
    given colon: TimeSeparation = () => t":"
    given none: TimeSeparation = () => t""
    given french: TimeSeparation = () => t"h"

package calendars:
  given julian: RomanCalendar:
    def leapYear(year: Annual): Boolean = year.int%4 == 0
    def leapYearsSinceEpoch(year: Year): Int = year.int/4

  given gregorian: RomanCalendar:
    def leapYear(year: Annual): Boolean =
      year.int%4 == 0 && year.int%100 != 0 || year.int%400 == 0

    def leapYearsSinceEpoch(year: Year): Int = year.int/4 - year.int/100 + year.int/400 + 1

def now()(using clock: Clock): Instant = clock()

def today()(using clock: Clock, calendar: RomanCalendar, timezone: Timezone): Date =
  (now() in timezone).date

given base60Extractable: [text <: Text] => (Text is Extractable into Int)
      =>  text is Extractable into Base60 =
  case As[Int](value: Base60) => value
  case _                      => Unset

given base24Extractable: [text <: Text] => (Text is Extractable into Int)
      =>  text is Extractable into Base24 =
  case As[Int](value: Base24) => value
  case _                      => Unset

enum TimeEvent:
  case ParseStart

extension (inline double: Double)
  inline def am: Clockface = ${Aviation.validTime('double, false)}
  inline def pm: Clockface = ${Aviation.validTime('double, true)}

extension (one: 1)
  def year: Timespan = Timespan(StandardTime.Year, 1)
  def month: Timespan = Timespan(StandardTime.Month, 1)
  def week: Timespan = Timespan(StandardTime.Week, 1)
  def day: Timespan = Timespan(StandardTime.Day, 1)
  def hour: Timespan = Timespan.fixed(StandardTime.Hour, 1)
  def minute: Timespan = Timespan.fixed(StandardTime.Minute, 1)
  def second: Timespan = Timespan.fixed(StandardTime.Second, 1)

extension (int: Int)
  def years: Timespan = Timespan(StandardTime.Year, int)
  def months: Timespan = Timespan(StandardTime.Month, int)
  def weeks: Timespan = Timespan(StandardTime.Week, int)
  def days: Timespan = Timespan(StandardTime.Day, int)
  def hours: Timespan = Timespan.fixed(StandardTime.Hour, int)
  def minutes: Timespan = Timespan.fixed(StandardTime.Minute, int)
  def seconds: Timespan = Timespan.fixed(StandardTime.Second, int)

extension (inline context: StringContext)
  inline def tz(): Timezone = ${Timezone.Tz.expand('context)}
