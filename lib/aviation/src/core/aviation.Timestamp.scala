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
┃    Soundness, version 0.27.0.                                                                    ┃
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
import distillate.*
import fulminate.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import spectacular.*

import errorDiagnostics.stackTraces

import java.time as jt

object Timestamp:
  import calendars.gregorian

  given (Clockface is Showable, Date is Showable) => Timestamp is Showable =
    timestamp => s"${timestamp.time.show}, ${timestamp.date.show}".tt

  given decodable: Tactic[TimestampError] => Timestamp is Decodable in Text = text =>
    text match
      case r"$year(\d{4})-$month(\d{2})-$day(\d{2})T$hour(\d{2}):$minute(\d{2}):$second(\d{2})" =>
        tend:
          case NumberError(_, _) => TimestampError(text)
          case DateError(_)      => TimestampError(text)

        . within:
            Timestamp
             (Date(year.decode[Int],
              Month(month.decode[Int]),
              day.decode[Int]),
              Clockface(Base24(hour.decode[Int]),
              Base60(minute.decode[Int]),
              Base60(second.decode[Int])))

      case value =>
        raise(TimestampError(value)) yet Timestamp(2000-Jan-1, Clockface(0, 0, 0))

case class Timestamp(date: Date, time: Clockface):
  def in(timezone: Timezone): Moment = Moment(date, time, timezone)

  def stdlib(using RomanCalendar): jt.LocalDateTime =
    jt.LocalDateTime.of
     (date.year, date.month.ordinal, date.day, time.hour, time.minute, time.second)
    . nn

  def instant(using timezone: Timezone, calendar: RomanCalendar): Instant =
    Instant(stdlib.atZone(timezone.stdlib).nn.toInstant.nn.toEpochMilli())
