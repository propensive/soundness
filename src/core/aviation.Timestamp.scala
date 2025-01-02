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

import contingency.*
import fulminate.*
import kaleidoscope.*
import spectacular.*

import errorDiagnostics.stackTraces

object Timestamp:
  import calendars.gregorian

  given (using Tactic[TimestampError]) => Decoder[Timestamp] = text =>
    text match
      case r"$year([0-9]{4})-$month([0-9]{2})-$day([0-9]{2})T$hour([0-9]{2}):$minute([0-9]{2}):$second([0-9]{2})" =>
        tend:
          case NumberError(_, _) => TimestampError(text)
          case DateError(_)      => TimestampError(text)

        . within:
            Timestamp
             (Date(year.decode[Int],
              MonthName(month.decode[Int]),
              day.decode[Int]),
              Clockface(Base24(hour.decode[Int]),
              Base60(minute.decode[Int]),
              Base60(second.decode[Int])))

      case value =>
        raise(TimestampError(value))
        Timestamp(2000-Jan-1, Clockface(0, 0, 0))

case class Timestamp(date: Date, time: Clockface):
  def in(timezone: Timezone): LocalTime = LocalTime(date, time, timezone)
