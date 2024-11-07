/*
    Aviation, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import hypotenuse.*
import symbolism.*
import contingency.*
import anticipation.*
import quantitative.*
import prepositional.*

import java.time as jt

object Timing:
  opaque type Instant = Long
  opaque type TaiInstant = Long

  object TaiInstant:
    erased given underlying: Underlying[TaiInstant, Long] = ###
    given Timing.TaiInstant is GenericInstant as generic:
      def instant(millisecondsSinceEpoch: Long): Timing.TaiInstant = millisecondsSinceEpoch
      def millisecondsSinceEpoch(instant: Timing.TaiInstant): Long = instant


  object Instant:
    def apply[InstantType: GenericInstant](instant: InstantType): Instant = of(instant.millisecondsSinceEpoch)

    erased given underlying: Underlying[Instant, Long] = ###
    def of(millis: Long): Instant = millis

    given Timing.Instant is GenericInstant as generic:
      def instant(millisecondsSinceEpoch: Long): Timing.Instant = millisecondsSinceEpoch
      def millisecondsSinceEpoch(instant: Timing.Instant): Long = instant

    inline given Instant is Orderable as orderable:
      inline def compare
          (inline left: Instant, inline right: Instant, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =
        if left == right then !strict else (left < right)^greaterThan

    given ordering: Ordering[Instant] = Ordering.Long

    given Instant is Addable by Duration into Instant as plus = new Addable:
      type Self = Instant
      type Result = Instant
      type Operand = Duration
      def add(instant: Instant, duration: Duration): Instant = instant + (duration.value/1000.0).toLong

    given Instant is Subtractable by Instant into Duration as minus = new Subtractable:
      type Self = Instant
      type Result = Duration
      type Operand = Instant
      def subtract(left: Instant, right: Instant): Duration = Quantity((left - right)/1000.0)

  type Duration = Quantity[Seconds[1]]

  object Duration:
    def of(millis: Long): Duration = Quantity(millis/1000.0)

    given Timing.Duration is (GenericDuration & SpecificDuration) as generic =
      new GenericDuration with SpecificDuration:
        type Self = Timing.Duration
        def duration(milliseconds: Long): Timing.Duration = Quantity(milliseconds.toDouble)
        def milliseconds(duration: Timing.Duration): Long = (duration.value*1000).toLong

  extension (instant: Instant)
    @targetName("to")
    infix def ~ (that: Instant): Period = Period(instant, that)

    def tai: TaiInstant = LeapSeconds.tai(instant)

    infix def in (using RomanCalendar)(timezone: Timezone): LocalTime =
      val zonedTime = jt.Instant.ofEpochMilli(instant).nn.atZone(jt.ZoneId.of(timezone.name.s)).nn

      val date = (zonedTime.getMonthValue: @unchecked) match
        case MonthName(month) => unsafely(Date(zonedTime.getYear, month, zonedTime.getDayOfMonth))

      val time = ((zonedTime.getHour, zonedTime.getMinute, zonedTime.getSecond): @unchecked) match
        case (Base24(hour), Base60(minute), Base60(second)) => Clockface(hour, minute, second)

      LocalTime(date, time, timezone)

  extension (duration: Duration)
    def from(instant: Instant): Period = Period(instant, Instant.plus.add(instant, duration))
