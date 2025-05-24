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
import hieroglyph.*
import prepositional.*
import spectacular.*
import vacuous.*

export Aviation2.{Instant, Duration}
export Aviation.{Date, Year, Day}
export Month.{Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec}

given realm: Realm = realm"aviation"

package dateFormats:
  private given calendar: RomanCalendar = calendars.gregorian

  given european: Date is Showable =
    import endianness.littleEndian, numerics.fixedWidth, separators.dot, years.full
    Date.showable.text(_)

  given american: Date is Showable =
    import endianness.middleEndian, numerics.fixedWidth, separators.slash, years.full
    Date.showable.text(_)

  given unitedKingdom: Date is Showable =
    import endianness.littleEndian, numerics.fixedWidth, separators.slash, years.full
    Date.showable.text(_)

  given southEastAsia: Date is Showable =
    import endianness.littleEndian, numerics.fixedWidth, separators.hyphen, years.full
    Date.showable.text(_)

  given iso8601: Date is Showable =
    import endianness.bigEndian, numerics.fixedWidth, separators.hyphen, years.full
    Date.showable.text(_)

  package endianness:
    given bigEndian: Endianness = Endianness.BigEndian
    given littleEndian: Endianness = Endianness.LittleEndian
    given middleEndian: Endianness = Endianness.MiddleEndian

  package numerics:
    given fixedWidth: DateNumerics = DateNumerics.FixedWidth
    given variableWidth: DateNumerics = DateNumerics.VariableWidth

  package separators:
    given slash: DateSeparation = () => t"/"
    given hyphen: DateSeparation = () => t"-"
    given dot: DateSeparation = () => t"."
    given space: DateSeparation = () => t" "

  package years:
    given twoDigits: Years = Years.TwoDigitYear
    given full: Years = Years.FullYear

  package weekdays:
    given english: Weekdays =
      case Weekday.Mon => t"Monday"
      case Weekday.Tue => t"Tuesday"
      case Weekday.Wed => t"Wednesday"
      case Weekday.Thu => t"Thursday"
      case Weekday.Fri => t"Friday"
      case Weekday.Sat => t"Saturday"
      case Weekday.Sun => t"Sunday"

    given englishShort: Weekdays =
      case Weekday.Mon => t"Mon"
      case Weekday.Tue => t"Tue"
      case Weekday.Wed => t"Wed"
      case Weekday.Thu => t"Thu"
      case Weekday.Fri => t"Fri"
      case Weekday.Sat => t"Sat"
      case Weekday.Sun => t"Sun"

    given oneLetterAmbiguous: Weekdays =
      case Weekday.Mon => t"M"
      case Weekday.Tue => t"T"
      case Weekday.Wed => t"W"
      case Weekday.Thu => t"T"
      case Weekday.Fri => t"F"
      case Weekday.Sat => t"S"
      case Weekday.Sun => t"S"

    given shortestUnambiguous: Weekdays =
      case Weekday.Mon => t"M"
      case Weekday.Tue => t"Tu"
      case Weekday.Wed => t"W"
      case Weekday.Thu => t"Th"
      case Weekday.Fri => t"F"
      case Weekday.Sat => t"Sa"
      case Weekday.Sun => t"Su"

    given twoLetter: Weekdays =
      case Weekday.Mon => t"Mo"
      case Weekday.Tue => t"Tu"
      case Weekday.Wed => t"We"
      case Weekday.Thu => t"Th"
      case Weekday.Fri => t"Fr"
      case Weekday.Sat => t"Sa"
      case Weekday.Sun => t"Su"

  package months:
    given english: Months =
      case Jan => t"January"
      case Feb => t"February"
      case Mar => t"March"
      case Apr => t"April"
      case May => t"May"
      case Jun => t"June"
      case Jul => t"July"
      case Aug => t"August"
      case Sep => t"September"
      case Oct => t"October"
      case Nov => t"November"
      case Dec => t"December"

    given englishShort: Months =
      case Jan => t"Jan"
      case Feb => t"Feb"
      case Mar => t"Mar"
      case Apr => t"Apr"
      case May => t"May"
      case Jun => t"Jun"
      case Jul => t"Jul"
      case Aug => t"Aug"
      case Sep => t"Sep"
      case Oct => t"Oct"
      case Nov => t"Nov"
      case Dec => t"Dec"

    given oneLetterAmbiguous: Months =
      case Jan => t"J"
      case Feb => t"F"
      case Mar => t"M"
      case Apr => t"A"
      case May => t"M"
      case Jun => t"J"
      case Jul => t"J"
      case Aug => t"A"
      case Sep => t"S"
      case Oct => t"O"
      case Nov => t"N"
      case Dec => t"D"

    given numeric: Months = _.numerical.show

    given twoDigit: Months = month =>
      import textMetrics.uniform
      month.numerical.show.pad(2, Rtl, '0')

package timeFormats:
  given military: Clockface is Showable =
    import hours.twentyFourHour, numerics.fixedWidth, separators.none, specificity.minutes
    Clockface.showable.text(_)

  given civilian: Clockface is Showable =
    import hours.twelveHour, meridiems.upper, numerics.fixedWidth, separators.colon
    import specificity.minutes

    Clockface.showable.text(_)

  given associatedPress: Clockface is Showable =
    import hours.twelveHour, meridiems.lowerPunctuated, numerics.variableWidth, separators.colon
    import specificity.minutes
    Clockface.showable.text(_)

  given french: Clockface is Showable =
    import hours.twentyFourHour, numerics.fixedWidth, separators.french, specificity.minutes
    Clockface.showable.text(_)

  given iso8601: Clockface is Showable =
    import hours.twentyFourHour, numerics.fixedWidth, separators.colon, specificity.seconds
    Clockface.showable.text(_)

  given ledger: Clockface is Showable =
    import hours.twentyFourHour, numerics.fixedWidth, separators.dot, specificity.minutes
    Clockface.showable.text(_)

  given railway: Clockface is Showable =
    import hours.twentyFourHour, numerics.fixedWidth, separators.colon, specificity.minutes
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

  package hours:
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

  package separators:
    given dot: TimeSeparation = () => t"."
    given colon: TimeSeparation = () => t":"
    given none: TimeSeparation = () => t""
    given french: TimeSeparation = () => t"h"

package calendars:
  given julian: RomanCalendar:
    def leapYear(year: Annual): Boolean = year()%4 == 0
    def leapYearsSinceEpoch(year: Year): Int = year()/4

  given gregorian: RomanCalendar:
    def leapYear(year: Annual): Boolean = year()%4 == 0 && year()%100 != 0 || year()%400 == 0
    def leapYearsSinceEpoch(year: Year): Int = year()/4 - year()/100 + year()/400 + 1

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
