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

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.t
import kaleidoscope.*
import prepositional.*
import quantitative.*
import spectacular.*
import symbolism.*

// `Timestamp` lives in its own representation object (distinct from `object internal`, which holds
// `Date`/`Year`/`Day`) so that, although it shares `Date`'s underlying `Long`, the two are
// *distinct* opaque types everywhere outside this object — otherwise their identically-named
// accessors would collide after erasure. A `Timestamp` is a zoneless point on the JDN-epoch grid,
// packed as `jdn*MillisPerDay + msOfDay`; a `Date` is exactly a `Timestamp` whose time-of-day is
// zero. It carries no timezone or absolute-instant meaning — grounding to an `Instant` is a
// `Moment`'s job.
object timestampInternal:
  opaque type Timestamp = Long

  private def underlying(timestamp: Timestamp): Long = timestamp

  // The shape of a `Timestamp - Timestamp` difference: a regular span of days/hours/mins/seconds.
  type Difference = Timespan of (aviation.internal.Day.type & Hour.type & Minute.type & Seconds[1])

  object Timestamp:
    def apply(date: Date, time: Clockface): Timestamp =
      date.jdn.toLong*aviation.internal.MillisPerDay +
        (time.hour*3600L + time.minute*60L + time.second)*1000L +
        time.nanos/1_000_000L

    given showable: (Clockface is Showable, Date is Showable) => Timestamp is Showable =
      timestamp => t"${timestamp.time.show}, ${timestamp.date.show}"

    // The civil difference of two timestamps, decomposed into days/hours/minutes/seconds by
    // truncated (sign-consistent) division. Nominal, calendar-free; mirrors `Date - Date`.
    given subtractable: (Timestamp is Subtractable by Timestamp to Difference) =
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

    given decodable: Tactic[TimestampError] => Timestamp is Decodable in Text = text =>
      import calendars.gregorianCalendar
      import errorDiagnostics.stackTracesDiagnostics

      text match
        case r"$yr(\d{4})-$mn(\d{2})-$dy(\d{2})[ T]$hr(\d{2}):$mi(\d{2}):$sc(\d{2})" =>
          whereas:
            case NumberError(_, _, _) => TimestampError(text, TimestampError.Reason.BadNumber)
            case TimeError(_)         => TimestampError(text, TimestampError.Reason.BadTime)

          . mitigate:
              Timestamp
                ( Date(yr.decode[Year], Month(mn.decode[Int]), Day(dy.decode[Int])),
                  Clockface
                    ( Base24(hr.decode[Int]),
                      Base60(mi.decode[Int]),
                      Base60(sc.decode[Int]) ) )

        case value =>
          abort(TimestampError(value, TimestampError.Reason.BadFormat))

  extension (timestamp: Timestamp)
    def date: Date = Date.julianDay(Math.floorDiv(timestamp, aviation.internal.MillisPerDay).toInt)

    def time: Clockface =
      val ms = Math.floorMod(timestamp, aviation.internal.MillisPerDay)

      Clockface
        ( Base24((ms/3_600_000L).toInt),
          Base60(((ms%3_600_000L)/60_000L).toInt),
          Base60(((ms%60_000L)/1000L).toInt),
          ((ms%1000L)*1_000_000L).toInt )

    def year(using calendar: Calendar): calendar.Annual = timestamp.date.year
    def month(using calendar: Calendar): calendar.Mensual = timestamp.date.month
    def day(using calendar: Calendar): calendar.Diurnal = timestamp.date.day
    def monthstamp(using RomanCalendar): Monthstamp = timestamp.date.monthstamp
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

    def instant(using timezone: Timezone, calendar: RomanCalendar): Instant =
      import abstractables.instantAbstractable
      Instant(timestamp.stdlib.atZone(timezone.stdlib).nn.toInstant.nn.toEpochMilli())
