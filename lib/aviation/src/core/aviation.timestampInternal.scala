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

import java.time as jt

import abacist.*
import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import hypotenuse.*
import kaleidoscope.*
import prepositional.*
import quantitative.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

// A `Timestamp` is a zoneless point on the millisecond-since-JDN-epoch grid, packed into one
// `Long` as `jdn*MillisPerDay + msOfDay`. Its precision is a phantom `Form` type member (set with
// the `in` preposition): `Timestamp in Minute` is a minute-precise point, and `Date = Timestamp in
// Day` is exactly a day-precise one. The opaque `Long` keeps it unboxed, and the `Form` refinement
// lets per-precision givens (a date `Showable` vs a date-time `Showable`) dispatch. A `Timestamp`
// carries no timezone or instant meaning — grounding to an `Instant` is a `Moment`'s job.
//
// It lives in its own object (not `object internal`, which holds `Year`/`Day`/…) so its accessors
// don't collide after erasure with those opaque types where they coincide as `Long`/`Int`.
object timestampInternal:
  opaque type Timestamp = Long

  // A `Date` is a day-precise `Timestamp` (time-of-day clamped to zero).
  type Date = Timestamp in Day

  private def underlying(timestamp: Timestamp): Long = timestamp

  // The shape of a `Timestamp - Timestamp` difference: a regular span of days/hours/mins/seconds.
  type Difference =
    Timespan of (aviation.internal.Day.type & Hour.type & Minute.type & Seconds[1])

  object Timestamp:
    def apply(date: Date, time: Clockface): Timestamp =
      date.jdn.toLong*aviation.internal.MillisPerDay +
        (time.hour*3600L + time.minute*60L + time.second)*1000L +
        time.nanos/1_000_000L

    // Date-time display (any precision): "time, date".
    given timestampShowable: (Clockface is Showable, Date is Showable) => Timestamp is Showable =
      timestamp => t"${timestamp.time.show}, ${timestamp.date.show}"

    // The civil difference of two timestamps, decomposed into days/hours/minutes/seconds by
    // truncated (sign-consistent) division. Nominal, calendar-free.
    given timestampSubtractable: (Timestamp is Subtractable by Timestamp to Difference) =
      (a, b) =>
        val diff = underlying(a) - underlying(b)
        val days = (diff/aviation.internal.MillisPerDay).toInt
        val afterDays = diff%aviation.internal.MillisPerDay
        val hours = (afterDays/3_600_000L).toInt
        val afterHours = afterDays%3_600_000L
        val minutes = (afterHours/60_000L).toInt
        val seconds = (afterHours%60_000L)/1000.0

        Timespan(days = days, hours = hours, minutes = minutes, seconds = Quantity(seconds))
        . asInstanceOf[Difference]

    given timestampDecodable: Tactic[TimestampError] => Timestamp is Decodable in Text = text =>
      import calendars.gregorianCalendar
      import errorDiagnostics.stackTracesDiagnostics

      text match
        case r"$yr(\d{4})-$mn(\d{2})-$dy(\d{2})[ T]$hr(\d{2}):$mi(\d{2}):$sc(\d{2})" =>
          mitigate:
            case NumberError(_, _, _) => TimestampError(text, TimestampError.Reason.BadNumber)
            case TimeError(_)         => TimestampError(text, TimestampError.Reason.BadTime)

          . protect:
              Timestamp
                ( Date(Year(yr.decode[Int]), Month(mn.decode[Int]), Day(dy.decode[Int])),
                  Clockface
                    ( Base24(hr.decode[Int]),
                      Base60(mi.decode[Int]),
                      Base60(sc.decode[Int]) ) )

        case value =>
          abort(TimestampError(value, TimestampError.Reason.BadFormat))

    inline given dateUnderlying: Underlying[Date, Long] = !!

    // Day arithmetic with `Quanta[Days[1]]`, and the civil `Date - Date` in whole days.
    given dateSubtractable: Date is Subtractable by Date to Quanta[Days[1]] = (end, start) =>
      (Quanta(((underlying(end) - underlying(start))/aviation.internal.MillisPerDay).toInt)
      : Quanta[Days[1]])

    given dateSubtractable2: Date is Subtractable by Quanta[Days[1]] to Date = (end, start) =>
      end.addDays(-start[Days])

    given dateAddable: Date is Addable by Quanta[Days[1]] to Date = (left, right) =>
      left.addDays(right[Days])

    given dateAddable2: Quanta[Days[1]] is Addable by Date to Date = (left, right) =>
      right.addDays(left[Days])

    // Date-only display (no time-of-day), honouring locale-ish formatting givens.
    given dateShowable: (Endianness, DateNumerics, DateSeparation, Years) => Date is Showable =
      date =>
        import DateNumerics.*, Years.*
        import textMetrics.uniformMetric

        given calendar: RomanCalendar = calendars.gregorianCalendar

        def pad(n: Int): Text = (n%100).show.pad(2, Rtl, '0')

        val year: Text = summon[Years] match
          case TwoDigitYear => pad(date.year())
          case FullYear     => date.year().show

        val month: Text = summon[DateNumerics] match
          case FixedWidth    => pad(date.month.numerical)
          case VariableWidth => date.month.numerical.show

        val day: Text = summon[DateNumerics] match
          case FixedWidth    => pad(date.day())
          case VariableWidth => date.day().show

        summon[Endianness].match
          case Endianness.LittleEndian => List(day, month, year)
          case Endianness.MiddleEndian => List(month, day, year)
          case Endianness.BigEndian    => List(year, month, day)

        . join(summon[DateSeparation].separator)

    given dateDecoder: Tactic[TimeError] => Date is Decodable in Text = value =>
      import calendars.gregorianCalendar

      value.cut(t"-").to(List) match
        case As[Int](year) :: As[Int](month) :: As[Int](day) :: Nil =>
          Date(Year(year), Month(month), Day(day))

        case cnt =>
          abort(TimeError(_.Format(value, Iso8601, Prim)(Iso8601.Issue.Digit)))

    given dateEncodable: RomanCalendar => Date is Encodable in Text = date =>
      import hieroglyph.textMetrics.uniformMetric

      List
        ( date.year().toString.tt,
          date.month.numerical.toString.tt.pad(2, Rtl, '0'),
          date.day().toString.tt.pad(2, Rtl, '0') )

      . join(t"-")

    inline given dateOrderable: Date is Orderable:
      inline def compare
        ( inline left:        Date,
          inline right:       Date,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        if left == right then !strict else (left < right)^greaterThan

    given dateOrdering: Ordering[Date] = Ordering.Long.asInstanceOf[Ordering[Date]]

    // `Orderable`'s `Self` is invariant, so `Date` (above) and `Timestamp` each need their own
    // instance even though the comparison is identical.
    inline given timestampOrderable: Timestamp is Orderable:
      inline def compare
        ( inline left:        Timestamp,
          inline right:       Timestamp,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        if left == right then !strict else (left < right)^greaterThan

    given timestampOrdering: Ordering[Timestamp] =
      Ordering.Long.asInstanceOf[Ordering[Timestamp]]

    given dateWorkingDays: Holidays => (hebdomad: Hebdomad) => Date is Addable:
      type Operand = WorkingDays
      type Result = Date

      def add(date: Date, days: WorkingDays): Date =
        def recur(current: Date, count: Int): Date =
          if count == 0 then
            if current.weekend || summon[Holidays].holiday(current).present
            then recur(current.addDays(1), 0)
            else current
          else
            val next = current.addDays(count)
            val holidays = summon[Holidays].between(current, next)
            val weekends = Weekday.all.to(List).filter(_.weekend)
            val weekendDays = weekends.map(Weekday.count(current, next, _)).sum
            val weekdayHolidays = holidays.filter(!_.date.weekend).length
            val skipped = weekdayHolidays + weekendDays
            recur(next, skipped)

        recur(date, days())

    given dateWorkingDaysSubtractable: Holidays => (hebdomad: Hebdomad) => Date is Subtractable:
      type Operand = WorkingDays
      type Result = Date

      def subtract(date: Date, days: WorkingDays): Date =
        def recur(current: Date, count: Int): Date =
          if count == 0 then
            if current.weekend || summon[Holidays].holiday(current).present
            then recur(current.addDays(-1), 0)
            else current
          else
            val previous = current.addDays(-count)
            val holidays = summon[Holidays].between(previous, current)
            val weekends = Weekday.all.to(List).filter(_.weekend)
            val weekendDays = weekends.map(Weekday.count(previous, current, _)).sum
            val weekdayHolidays = holidays.filter(!_.date.weekend).length
            val skipped = weekdayHolidays + weekendDays
            recur(previous, skipped)

        recur(date, days())

  object Date:
    def julianDay(day: Int): Date = (day.toLong*aviation.internal.MillisPerDay).asInstanceOf[Date]

    def apply(using calendar: Calendar)
      ( year: calendar.Annual, month: calendar.Mensual, day: calendar.Diurnal )
    :   Date raises TimeError =

      calendar.jdn(year, month, day)

    trait Format(val name: Text):
      type Issue: Communicable

  extension (timestamp: Timestamp)
    def date: Date =
      Date.julianDay(Math.floorDiv(underlying(timestamp), aviation.internal.MillisPerDay).toInt)

    def time: Clockface =
      val ms = Math.floorMod(underlying(timestamp), aviation.internal.MillisPerDay)

      Clockface
        ( Base24((ms/3_600_000L).toInt),
          Base60(((ms%3_600_000L)/60_000L).toInt),
          Base60(((ms%60_000L)/1000L).toInt),
          ((ms%1000L)*1_000_000L).toInt )

    def jdn: Int = Math.floorDiv(underlying(timestamp), aviation.internal.MillisPerDay).toInt
    def year(using calendar: Calendar): calendar.Annual = calendar.annual(timestamp.date)
    def month(using calendar: Calendar): calendar.Mensual = calendar.mensual(timestamp.date)
    def day(using calendar: Calendar): calendar.Diurnal = calendar.diurnal(timestamp.date)
    def weekday: Weekday = Weekday.fromOrdinal(timestamp.jdn%7)
    def weekend(using hebdomad: Hebdomad): Boolean = timestamp.weekday.weekend

    def anniversary: Anniversary =
      Anniversary
        ( calendars.gregorianCalendar.mensual(timestamp.date),
          calendars.gregorianCalendar.diurnal(timestamp.date) )

    def monthstamp(using calendar: RomanCalendar): Monthstamp =
      Monthstamp(calendar.annual(timestamp.date), calendar.mensual(timestamp.date))

    def yearDay(using calendar: Calendar): Int =
      timestamp.jdn - calendar.zerothDayOfYear(calendar.annual(timestamp.date)).jdn

    def hour: Int = timestamp.time.hour
    def minute: Int = timestamp.time.minute
    def second: Int = timestamp.time.second
    def in(timezone: Timezone): Moment = Moment(timestamp.date, timestamp.time, timezone)

    def stdlib(using RomanCalendar): jt.LocalDateTime =
      jt.LocalDateTime.of
        ( timestamp.date.year(),
          timestamp.date.month.numerical,
          timestamp.date.day(),
          timestamp.time.hour,
          timestamp.time.minute,
          timestamp.time.second,
          timestamp.time.nanos )

      . nn

    def instant(using timezone: Timezone, calendar: RomanCalendar): Instant over Posix =
      Instant.of[Posix](timestamp.stdlib.atZone(timezone.stdlib).nn.toInstant.nn.toEpochMilli())

  // Comparisons are defined on `Timestamp` (the whole grid is monotonic); `Date`, being a
  // `Timestamp in Day`, inherits them.
  extension (timestamp: Timestamp)
    @targetName("gt")
    infix def > (right: Timestamp): Boolean = underlying(timestamp) > underlying(right)

    @targetName("lt")
    infix def < (right: Timestamp): Boolean = underlying(timestamp) < underlying(right)

    @targetName("lte")
    infix def <= (right: Timestamp): Boolean = underlying(timestamp) <= underlying(right)

    @targetName("gte")
    infix def >= (right: Timestamp): Boolean = underlying(timestamp) >= underlying(right)

  extension (date: Date)
    def addDays(count: Int): Date =
      (underlying(date) + count.toLong*aviation.internal.MillisPerDay).asInstanceOf[Date]
