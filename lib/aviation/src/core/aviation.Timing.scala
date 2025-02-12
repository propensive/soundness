                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                    ╭───╮                                         ┃
┃  ╭─────────╮                                       │   │                                         ┃
┃  │   ╭─────╯╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮  ┃
┃  │   ╰─────╮│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯  ┃
┃  ╰─────╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ├╌╯╌─╯╰─╌ ╰───╮╰─╌ ╰───╮  ┃
┃  ╭─────╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╌   │╭───╌   │  ┃
┃  ╰─────────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯  ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0. © Copyright 2023-25 Jon Pretty, Propensive OÜ.                     ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        http://www.apache.org/licenses/LICENSE-2.0                                                ┃
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
import hypotenuse.*
import prepositional.*
import proscenium.*
import quantitative.*
import rudiments.*
import symbolism.*

import java.time as jt

object Timing:
  opaque type Instant = Long
  opaque type TaiInstant = Long

  object TaiInstant:
    erased given underlying: Underlying[TaiInstant, Long] = ###
    given generic: Timing.TaiInstant is (Abstractable & Instantiable) across Instants into
                    Long from Long =
      new Abstractable with Instantiable:
        type Self = Timing.TaiInstant
        type Source = Long
        type Result = Long
        type Domain = Instants
        def apply(long: Long): Timing.TaiInstant = long
        def genericize(instant: Timing.TaiInstant): Long = instant


  object Instant:
    def apply[InstantType: Abstractable across Instants into Long](instant: InstantType): Instant =
      of(instant.generic)

    erased given underlying: Underlying[Instant, Long] = ###
    def of(millis: Long): Instant = millis

    given generic: Timing.Instant is (Abstractable & Instantiable) across Instants into Long from
                    Long =
      new Abstractable with Instantiable:
        type Self = Timing.Instant
        type Result = Long
        type Source = Long
        type Domain = Instants
        def apply(long: Long): Timing.Instant = long
        def genericize(instant: Timing.Instant): Long = instant

    inline given orderable: Instant is Orderable:
      inline def compare
         (inline left: Instant,
          inline right: Instant,
          inline strict: Boolean,
          inline greaterThan: Boolean)
      :     Boolean =
        if left == right then !strict else (left < right)^greaterThan

    given ordering: Ordering[Instant] = Ordering.Long

    given plus: Instant is Addable by Duration into Instant = new Addable:
      type Self = Instant
      type Result = Instant
      type Operand = Duration
      def add(instant: Instant, duration: Duration): Instant =
        instant + (duration.value/1000.0).toLong

    given minus: Instant is Subtractable by Instant into Duration = new Subtractable:
      type Self = Instant
      type Result = Duration
      type Operand = Instant
      def subtract(left: Instant, right: Instant): Duration = Quantity((left - right)/1000.0)

  type Duration = Quantity[Seconds[1]]

  object Duration:
    def of(millis: Long): Duration = Quantity(millis/1000.0)

    given generic: Timing.Duration is (GenericDuration & SpecificDuration) =
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

      val date = zonedTime.getMonthValue.absolve match
        case MonthName(month) => unsafely(Date(zonedTime.getYear, month, zonedTime.getDayOfMonth))

      val time = (zonedTime.getHour, zonedTime.getMinute, zonedTime.getSecond).absolve match
        case (Base24(hour), Base60(minute), Base60(second)) => Clockface(hour, minute, second)

      LocalTime(date, time, timezone)

    def long: Long = instant

  extension (duration: Duration)
    def from(instant: Instant): Period = Period(instant, Instant.plus.add(instant, duration))
