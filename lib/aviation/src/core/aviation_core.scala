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
import fulminate.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import quantitative.Radix
import spectacular.*
import symbolism.*
import vacuous.*

export protointernal.{Instant, Duration}
export aviation.internal.{Year, Day, Anniversary, WorkingDays}
export aviation.timestampInternal.{Timestamp, Date, Monthstamp}
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

// Phantom precision markers for `Timestamp in <unit>` (the `Form` type member). `Day`, `Year` and
// `Month` already exist as types and double as markers; the sub-day units exist only as radix
// objects, so these traits give them a type to name. They are erased — purely type-level tags.
trait Hour
trait Minute
trait Second

// Phantom chronometry markers for `Instant over <chronometry>` (the `Transport` type member): which
// timeline an instant's `Long` counts on. `Tai` is atomic time; `Posix` is leap-free Unix time;
// `Monotonic` is a `System.nanoTime` reading (nanosecond resolution, arbitrary origin). `Tai` and
// `Posix` have a `Chronometry` (so they ground/convert); `Monotonic` does not — with no fixed
// relation to wall-clock time it only measures elapsed time. Purely type-level tags.
trait Tai
trait Posix
trait Monotonic

package instantDecodables:
  given iso8601InstantDecodable: Tactic[TimeError] => (Instant over Posix) is Decodable in Text =
    Iso8601.parse(_)

  given rfc1123InstantDecodable: Tactic[TimeError] => (Instant over Posix) is Decodable in Text =
    Rfc1123.parse(_)

package dateFormats:
  private given calendar: RomanCalendar = calendars.gregorianCalendar

  given europeanDateFormat: Date is Showable =
    import endianness.littleEndian, numerics.fixedWidthDateNumerics, separators.dotDateSeparator
    import years.fullYears
    Timestamp.dateShowable.text(_)

  given americanDateFormat: Date is Showable =
    import endianness.middleEndian, numerics.fixedWidthDateNumerics, separators.slashDateSeparator
    import years.fullYears
    Timestamp.dateShowable.text(_)

  given unitedKingdomDateFormat: Date is Showable =
    import endianness.littleEndian, numerics.fixedWidthDateNumerics, separators.slashDateSeparator
    import years.fullYears
    Timestamp.dateShowable.text(_)

  given southEastAsiaDateFormat: Date is Showable =
    import endianness.littleEndian, numerics.fixedWidthDateNumerics, separators.hyphenDateSeparator
    import years.fullYears
    Timestamp.dateShowable.text(_)

  given iso8601DateFormat: Date is Showable =
    import endianness.bigEndian, numerics.fixedWidthDateNumerics, separators.hyphenDateSeparator
    import years.fullYears
    Timestamp.dateShowable.text(_)

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

// A human-readable, relative rendering of a `Timespan`, in place of the default ISO-8601 duration:
// "in 18 minutes", "in 1 hour and 6 seconds", "8 minutes ago", "12 hours and 38 minutes ago", and
// "just now" for a zero span. Only the non-zero components are shown (coarsest first); the sign
// chooses "in …" / "… ago". `import timespanFormats.relativeTimespan` to use it.
package timespanFormats:
  given relativeTimespan: Timespan is Showable = timespan =>
    val fields: List[(Long, Text, Text)] =
      List
        ( (timespan.years.toLong,         t"year",   t"years"),
          (timespan.months.toLong,        t"month",  t"months"),
          (timespan.weeks.toLong,         t"week",   t"weeks"),
          (timespan.days.toLong,          t"day",    t"days"),
          (timespan.hours.toLong,         t"hour",   t"hours"),
          (timespan.minutes.toLong,       t"minute", t"minutes"),
          (timespan.seconds.value.toLong, t"second", t"seconds") )

    val nonZero = fields.filter(_._1 != 0)

    if nonZero.isEmpty then t"just now" else
      val rendered = nonZero.map: field =>
        val magnitude = field._1.abs
        t"$magnitude ${if magnitude == 1 then field._2 else field._3}"

      val joined =
        rendered match
          case List(one) => one
          case many      => t"${many.init.join(t", ")} and ${many.last}"

      if nonZero.head._1 < 0 then t"$joined ago" else t"in $joined"

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
  given persianCalendar: PersianCalendar = PersianCalendar()
  given indianCalendar: IndianCalendar = IndianCalendar()
  given hebrewCalendar: HebrewCalendar = HebrewCalendar()
  given frenchRepublicanCalendar: FrenchRepublicanCalendar = FrenchRepublicanCalendar()

  given buddhistCalendar: OffsetCalendar = OffsetCalendar(gregorianCalendar, 543, t"Buddhist")
  given minguoCalendar: OffsetCalendar = OffsetCalendar(gregorianCalendar, -1911, t"Minguo")

  // Year + day-of-year (no months); construct with `OrdinalCalendar(year, dayOfYear)`.
  given ordinalCalendar: OrdinalCalendar.type = OrdinalCalendar

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

// The default interpretation of an `Instant`'s `Long` (used by `Instant(…)`, decoding, etc.).
// Import one of these to choose the timeline bare instants count on; convert with `.over[…]`.
package chronometries:
  given posix: (Chronometry.Ambient { type Transport = Posix }) =
    new Chronometry.Ambient:
      type Transport = Posix
      def chronometry: Posix is Chronometry = Chronometry.posix

  given atomic: (Chronometry.Ambient { type Transport = Tai }) =
    new Chronometry.Ambient:
      type Transport = Tai
      def chronometry: Tai is Chronometry = Chronometry.tai

// Overrides for the spring-forward DST gap, used to ground a `Moment` whose wall-clock time doesn't
// exist. The ambient default is `GapPolicy.pushForward` (matching `java.time`); import one of these
// to push the other way or to reject the gap.
package gapPolicies:
  given pushBackward: GapPolicy = (_, backward) => backward

  given rejectGap: Tactic[TimeError] => GapPolicy =
    (_, _) => abort(TimeError(_.Gap))

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
      abort(TimeError(_.Invalid(year(), calendar.monthOrdinal(year, month) + 1, day, calendar)))

def now()(using clock: Clock): Instant over Posix = clock()

// A reading of the monotonic system clock (`System.nanoTime`), for measuring elapsed time. It has
// an arbitrary origin, so it can only be compared/subtracted with other `Monotonic` readings, never
// grounded to a calendar instant.
def monotonic(): Instant over Monotonic = Instant.of[Monotonic](System.nanoTime)

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


object TimeEvent:
  given communicable: TimeEvent is Communicable =
    case ParseTzdb(name) => m"parsing the timezone database file $name"

enum TimeEvent:
  case ParseTzdb(name: Text)

// Which occurrence of a wall-clock time a `Moment` denotes during a fall-back DST overlap, where
// the same local time happens twice (clocks go back). `First` is the earlier instant (under the
// offset before the transition); `Second` is the later one (after it). For unambiguous times it is
// always `First`, and grounding ignores it.
enum Occurrence derives CanEqual:
  case First, Second

// Whether a `Moment` is the leap second inserted at the end of some UTC days (`23:59:60`). A
// `Clockface` second is a `Base60` (0–59), so an inserted leap second is stored as `:59` with this
// flag set; only a `Moment` grounds or renders it as the 61st second. `None` is every other moment.
enum Leap derives CanEqual:
  case None, Inserted

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

  transparent inline def dur: Interpolation = interpolation[Timespan](context)

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
