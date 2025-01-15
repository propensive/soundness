/*
    Aviation, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import rudiments.*

import java.time as jt

object LocalTime:
  given (using RomanCalendar) => LocalTime is GenericInstant as generic =
    _.instant.millisecondsSinceEpoch

case class LocalTime(date: Date, time: Clockface, timezone: Timezone):
  def instant(using RomanCalendar): Instant =
    val ldt =
      jt.LocalDateTime.of
       (date.year, date.month.numerical, date.day, time.hour, time.minute, time.second)

    Instant.of(ldt.nn.atZone(jt.ZoneId.of(timezone.name.s)).nn.toInstant.nn.toEpochMilli)
