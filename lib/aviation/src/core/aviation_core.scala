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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import distillate.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import quantitative.Radix
import spectacular.*
import symbolism.*
import vacuous.*

export protointernal.{Instant, Duration}
export aviation.internal.{Date, Year, Day, Anniversary, WorkingDays}
export Month.{Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec}

// A `MonthRadix` is the "month" radix of some calendar (Gregorian `Month`, `IslamicMonth`,
// `CopticMonth`, …). Each is `Irregular` and distinctly-typed, so a span counted in one calendar's
// months can only be added to a date in that calendar — cross-calendar mixing is a compile error.
trait MonthRadix extends Radix.Irregular

// Radices not already represented by an existing companion. `Week`/`Hour`/`Minute` are `Regular`
// (constant ratio to the radix below); `Year` is `Irregular`; months are `MonthRadix`.
object Week extends Radix.Regular:
  given multiplicable: Int is Multiplicable by Week.type to (Timespan of Week.type) =
    (n, _) => Timespan(Week, n)

object Hour extends Radix.Regular:
  given multiplicable: Int is Multiplicable by Hour.type to (Timespan of Hour.type) =
    (n, _) => Timespan(Hour, n)

object Minute extends Radix.Regular:
  given multiplicable: Int is Multiplicable by Minute.type to (Timespan of Minute.type) =
    (n, _) => Timespan(Minute, n)

package instantDecodables:
  given iso8601InstantDecodable: Tactic[TimeError] => Instant is Decodable in Text =
    Iso8601.parse(_)

  given rfc1123InstantDecodable: Tactic[TimeError] => Instant is Decodable in Text =
    Rfc1123.parse(_)

package dateFormats:
  private given calendar: RomanCalendar = calendars.gregorianCalendar

  given europeanDateFormat: Date is Showable =
    import endianness.littleEndian, numerics.fixedWidthDateNumerics, separators.dotDateSeparator
    import years.fullYears
    Date.showable.text(_)

  given americanDateFormat: Date is Showable =
    import endianness.middleEndian, numerics.fixedWidthDateNumerics, separators.slashDateSeparator
    import years.fullYears
    Date.showable.text(_)

  given unitedKingdomDateFormat: Date is Showable =
    import endianness.littleEndian, numerics.fixedWidthDateNumerics, separators.slashDateSeparator
    import years.fullYears
    Date.showable.text(_)

  given southEastAsiaDateFormat: Date is Showable =
    import endianness.littleEndian, numerics.fixedWidthDateNumerics, separators.hyphenDateSeparator
    import years.fullYears
    Date.showable.text(_)

  given iso8601DateFormat: Date is Showable =
    import endianness.bigEndian, numerics.fixedWidthDateNumerics, separators.hyphenDateSeparator
    import years.fullYears
    Date.showable.text(_)

  package endianness:
    given bigEndian: Endianness = Endianness.BigEndian
    given littleEndian: Endianness = Endianness.LittleEndian
    given middleEndian: Endianness = Endianness.MiddleEndian

  package numerics:
    given fixedWidthDateNumerics: DateNumerics = DateNumerics.FixedWidth
    given variableWidthDateNumerics: DateNumerics = DateNumerics.VariableWidth

  package separators:
    given slashDateSeparator: DateSeparation = () => t"/"
    given hyphenDateSeparator: DateSeparation = () => t"-"
    given dotDateSeparator: DateSeparation = () => t"."
    given spaceDateSeparator: DateSeparation = () => t" "

  package years:
    given twoDigitsYears: Years = Years.TwoDigitYear
    given fullYears: Years = Years.FullYear

  package weekdays:
    given englishWeekdays: Weekdays =
      case Weekday.Mon => t"Monday"
      case Weekday.Tue => t"Tuesday"
      case Weekday.Wed => t"Wednesday"
      case Weekday.Thu => t"Thursday"
      case Weekday.Fri => t"Friday"
      case Weekday.Sat => t"Saturday"
      case Weekday.Sun => t"Sunday"

    given englishShortWeekdays: Weekdays =
      case Weekday.Mon => t"Mon"
      case Weekday.Tue => t"Tue"
      case Weekday.Wed => t"Wed"
      case Weekday.Thu => t"Thu"
      case Weekday.Fri => t"Fri"
      case Weekday.Sat => t"Sat"
      case Weekday.Sun => t"Sun"

    given oneLetterAmbiguousWeekdays: Weekdays =
      case Weekday.Mon => t"M"
      case Weekday.Tue => t"T"
      case Weekday.Wed => t"W"
      case Weekday.Thu => t"T"
      case Weekday.Fri => t"F"
      case Weekday.Sat => t"S"
      case Weekday.Sun => t"S"

    given shortestUnambiguousWeekdays: Weekdays =
      case Weekday.Mon => t"M"
      case Weekday.Tue => t"Tu"
      case Weekday.Wed => t"W"
      case Weekday.Thu => t"Th"
      case Weekday.Fri => t"F"
      case Weekday.Sat => t"Sa"
      case Weekday.Sun => t"Su"

    given twoLetterWeekdays: Weekdays =
      case Weekday.Mon => t"Mo"
      case Weekday.Tue => t"Tu"
      case Weekday.Wed => t"We"
      case Weekday.Thu => t"Th"
      case Weekday.Fri => t"Fr"
      case Weekday.Sat => t"Sa"
      case Weekday.Sun => t"Su"

  package months:
    given englishMonths: Months =
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

    given englishShortMonths: Months =
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

    given oneLetterAmbiguousMonths: Months =
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

    given numericMonths: Months = _.numerical.show

    given twoDigitMonths: Months = month =>
      import textMetrics.uniformMetric
      month.numerical.show.pad(2, Rtl, '0')

package timeFormats:
  given militaryTimeFormat: Clockface is Showable =
    import hours.twentyFourHourClock, numerics.fixedWidthTimeNumerics, separators.noneTimeSeparator
    import specificity.minutesSpecificity
    Clockface.showable.text(_)

  given civilianTimeFormat: Clockface is Showable =
    import hours.twelveHourClock, meridiems.upperMeridiem, numerics.fixedWidthTimeNumerics
    import separators.colonTimeSeparator
    import specificity.minutesSpecificity

    Clockface.showable.text(_)

  given associatedPressTimeFormat: Clockface is Showable =
    import hours.twelveHourClock, meridiems.lowerPunctuatedMeridiem
    import numerics.variableWidthTimeNumerics, separators.colonTimeSeparator
    import specificity.minutesSpecificity
    Clockface.showable.text(_)

  given frenchTimeFormat: Clockface is Showable =
    import hours.twentyFourHourClock, numerics.fixedWidthTimeNumerics
    import separators.frenchTimeSeparator, specificity.minutesSpecificity
    Clockface.showable.text(_)

  given iso8601TimeFormat: Clockface is Showable =
    import hours.twentyFourHourSecondsClock, numerics.fixedWidthTimeNumerics
    import separators.colonTimeSeparator, specificity.secondsSpecificity
    Clockface.showable.text(_)

  given ledgerTimeFormat: Clockface is Showable =
    import hours.twentyFourHourClock, numerics.fixedWidthTimeNumerics, separators.dotTimeSeparator
    import specificity.minutesSpecificity
    Clockface.showable.text(_)

  given railwayTimeFormat: Clockface is Showable =
    import hours.twentyFourHourClock, numerics.fixedWidthTimeNumerics, separators.colonTimeSeparator
    import specificity.minutesSpecificity
    Clockface.showable.text(_)

  package meridiems:
    given upperMeridiem: Meridiem is Showable =
      case Meridiem.Am => t"AM"
      case Meridiem.Pm => t"PM"

    given lowerMeridiem: Meridiem is Showable =
      case Meridiem.Am => t"am"
      case Meridiem.Pm => t"pm"

    given upperPunctuatedMeridiem: Meridiem is Showable =
      case Meridiem.Am => t"A.M."
      case Meridiem.Pm => t"P.M."

    given lowerPunctuatedMeridiem: Meridiem is Showable =
      case Meridiem.Am => t"a.m."
      case Meridiem.Pm => t"p.m."

  package hours:
    given twelveHourClock: (Meridiem is Showable) => TimeFormat:
      def postfix(meridiem: Meridiem): Text = t" ${meridiem}"
      def halfDay: Boolean = true
      def seconds: Boolean = false

    given twelveHourSecondsClock: (Meridiem is Showable) => TimeFormat:
      def postfix(meridiem: Meridiem): Text = t" ${meridiem}"
      def halfDay: Boolean = true
      def seconds: Boolean = false

    given twentyFourHourClock: TimeFormat:
      def postfix(meridiem: Meridiem): Text = t""
      def halfDay: Boolean = false
      def seconds: Boolean = false

    given twentyFourHourSecondsClock: TimeFormat:
      def postfix(meridiem: Meridiem): Text = t""
      def halfDay: Boolean = false
      def seconds: Boolean = true

  package specificity:
    given minutesSpecificity: TimeSpecificity = TimeSpecificity.Minutes
    given secondsSpecificity: TimeSpecificity = TimeSpecificity.Seconds

  package numerics:
    given fixedWidthTimeNumerics: TimeNumerics = TimeNumerics.FixedWidth
    given variableWidthTimeNumerics: TimeNumerics = TimeNumerics.VariableWidth

  package separators:
    given dotTimeSeparator: TimeSeparation = () => t"."
    given colonTimeSeparator: TimeSeparation = () => t":"
    given noneTimeSeparator: TimeSeparation = () => t""
    given frenchTimeSeparator: TimeSeparation = () => t"h"

package calendars:
  given julianCalendar: RomanCalendar(t"Julian"):
    def leapYear(year: Annual): Boolean = year()%4 == 0
    def leapYearsSinceEpoch(year: Year): Int = year()/4

  given gregorianCalendar: RomanCalendar("Gregorian"):
    def leapYear(year: Annual): Boolean = year()%4 == 0 && year()%100 != 0 || year()%400 == 0
    def leapYearsSinceEpoch(year: Year): Int = year()/4 - year()/100 + year()/400 + 1

  given copticCalendar: CopticCalendar = CopticCalendar()
  given ethiopianCalendar: EthiopianCalendar = EthiopianCalendar()
  given islamicCalendar: IslamicCalendar = IslamicCalendar()
  given frenchRepublicanCalendar: FrenchRepublicanCalendar = FrenchRepublicanCalendar()

  // The Julian-to-Gregorian cutovers of the two best-known reforms, as two-segment `Regime`s. The
  // first day of each segment is given as a Julian day number; the gap between (the dates in
  // neither calendar) is rejected. Provided for explicit import, like the calendars above.
  given papalCutover: Regime =
    Regime
      ( t"Papal",
        Regime.Segment(Date.julianDay(Int.MinValue), julianCalendar),
        Regime.Segment(Date.julianDay(2299161), gregorianCalendar) )

  given britishCutover: Regime =
    Regime
      ( t"British",
        Regime.Segment(Date.julianDay(Int.MinValue), julianCalendar),
        Regime.Segment(Date.julianDay(2361222), gregorianCalendar) )

  package nonexistentLeapDays:
    given roundUpLeapDay: Anniversary.NonexistentLeapDay = year =>
      import calendars.gregorianCalendar
      unsafely(Date(year, Mar, Day(1)))

    given roundDownLeapDay: Anniversary.NonexistentLeapDay = year =>
      import calendars.gregorianCalendar
      unsafely(Date(year, Feb, Day(28)))

    given raiseErrorsLeapDay: Tactic[TimeError] => Anniversary.NonexistentLeapDay = year =>
      import calendars.gregorianCalendar
      unsafely(Date(year, Feb, Day(29)))

// Month-end overflow policies for adding months/years to a date (e.g. Jan 31 + 1 month). No default
// is provided, so such arithmetic requires one of these to be imported.
package monthEnds:
  given clampMonthEnd: Disambiguation = new Disambiguation:
    def resolve(using calendar: Calendar)(year: Year, month: calendar.Mensual, day: Int): Date =
      unsafely(Date(year, month, Day(day.min(calendar.daysInMonth(month, year)))))

  given overflowMonthEnd: Disambiguation = new Disambiguation:
    def resolve(using calendar: Calendar)(year: Year, month: calendar.Mensual, day: Int): Date =
      val max = calendar.daysInMonth(month, year)
      unsafely(Date(year, month, Day(max))).addDays(day - max)

  given raiseMonthEnd: Tactic[TimeError] => Disambiguation = new Disambiguation:
    def resolve(using calendar: Calendar)(year: Year, month: calendar.Mensual, day: Int): Date =
      abort(TimeError(_.Invalid(year(), calendar.monthOrdinal(month) + 1, day, calendar)))

def now()(using clock: Clock): Instant = clock()

def today()(using clock: Clock, calendar: RomanCalendar, timezone: Timezone): Date =
  (now() in timezone).date


given base60Extractable: [text <: Text] => (Text is Extractable to Int)
=>  text is Extractable to Base60 =

  case As[Int](value: Base60) => value
  case _                      => Unset


given base24Extractable: [text <: Text] => (Text is Extractable to Int)
=>  text is Extractable to Base24 =

  case As[Int](value: Base24) => value
  case _                      => Unset


enum TimeEvent:
  case ParseStart

extension (inline double: Double)
  inline def am: Clockface = ${aviation.internal.validTime('double, false)}
  inline def pm: Clockface = ${aviation.internal.validTime('double, true)}

// The result type is decided by the literal's precision: a year, year-month, date,
// zoneless date-time, or zoned date-time (also the result for any RFC 1123 literal).
inline given tsInterpolator: (Year | Monthstamp | Date | Timestamp | Moment) is Interpolable:
  transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
    ( inline insertions: Any* )
  :   Year | Monthstamp | Date | Timestamp | Moment =

    ${aviation.internal.tsInterpolator[parts]('insertions)}

extension (inline context: StringContext)
  transparent inline def tz: Interpolation = interpolation[Timezone](context)

  transparent inline def ts: Interpolation =
    interpolation[Year | Monthstamp | Date | Timestamp | Moment](context)

export Weekday.{Mon, Tue, Wed, Thu, Fri, Sat, Sun}

package hebdomads:
  given europeanHebdomad: Hebdomad:
    def start: Weekday = Weekday.Mon
    def weekend(day: Weekday): Boolean = day.ordinal >= 5

  given northAmericanHebdomad: Hebdomad:
    def start: Weekday = Weekday.Sun
    def weekend(day: Weekday): Boolean = day.ordinal >= 5

  given jewishHebdomad: Hebdomad:
    def start: Weekday = Weekday.Sun
    def weekend(day: Weekday): Boolean = day == Weekday.Fri || day == Weekday.Sat

type Base60 =
  0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
    21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 |
    40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 |
    59

type Base24 =
  0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
    21 | 22 | 23
