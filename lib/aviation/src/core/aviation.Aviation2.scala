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
┃    Soundness, version 0.32.0.                                                                    ┃
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
import gossamer.*
import hypotenuse.*
import kaleidoscope.*
import prepositional.*
import proscenium.*
import quantitative.*
import rudiments.*
import symbolism.*
import vacuous.*

object Aviation2:
  opaque type Instant = Long
  opaque type TaiInstant = Long

  object TaiInstant:
    erased given underlying: Underlying[TaiInstant, Long] = !!

    given generic: Aviation2.TaiInstant is (Abstractable & Instantiable) across Instants into
                    Long from Long =
      new Abstractable with Instantiable:
        type Self = Aviation2.TaiInstant
        type Source = Long
        type Result = Long
        type Domain = Instants
        def apply(long: Long): Aviation2.TaiInstant = long
        def genericize(instant: Aviation2.TaiInstant): Long = instant

  object InstantSubtractable:
    given instant: Instant is InstantSubtractable into Duration = new InstantSubtractable:
      type Self = Instant
      type Result = Duration
      def subtract(left: Instant, right: Instant): Duration = Quantity((left - right)/1000.0)

    given duration: [units <: Measure: Normalizable into Seconds[1]]
          => Quantity[units] is InstantSubtractable into Instant = new InstantSubtractable:
      type Self = Quantity[units]
      type Result = Instant

      def subtract(left: Instant, right: Quantity[units]): Instant =
        left - (right.normalize.value*1000.0).toLong

  trait InstantSubtractable:
    type Self
    type Result
    def subtract(left: Instant, right: Self): Result

  object Instant:
    final val Min: Instant = Long.MinValue
    final val Max: Instant = Long.MaxValue

    def apply[instant: Abstractable across Instants into Long](instant: instant): Instant =
      of(instant.generic)

    erased given underlying: Underlying[Instant, Long] = !!
    def of(millis: Long): Instant = millis

    given generic: Aviation2.Instant is (Abstractable & Instantiable) across Instants into Long from
                    Long =
      new Abstractable with Instantiable:
        type Self = Aviation2.Instant
        type Result = Long
        type Source = Long
        type Domain = Instants
        def apply(long: Long): Aviation2.Instant = long
        def genericize(instant: Aviation2.Instant): Long = instant

    inline given orderable: Instant is Orderable:
      inline def compare
                  (inline left:        Instant,
                   inline right:       Instant,
                   inline strict:      Boolean,
                   inline greaterThan: Boolean)
      : Boolean =

          if left.long == right.long then !strict else (left.long < right.long)^greaterThan

    given ordering: Ordering[Instant] = Ordering.Long

    given plus: [units <: Measure: Normalizable into Seconds[1]]
          => Instant is Addable by Quantity[units] into Instant = new Addable:
      type Self = Instant
      type Operand = Quantity[units]
      type Result = Instant

      def add(instant: Instant, duration: Quantity[units]): Instant =
        instant + (duration.normalize.value*1000.0).toLong

    given minus: [operand: InstantSubtractable]
          =>  Instant is Subtractable by operand into operand.Result =
      operand.subtract(_, _)

  type Duration = Quantity[Seconds[1]]

  object Duration:
    def of(millis: Long): Duration = Quantity(millis/1000.0)

    given generic: [units <: Measure: Normalizable into Seconds[1]]
          =>  Quantity[units] is (GenericDuration & SpecificDuration) =
      new GenericDuration with SpecificDuration:
        type Self = Quantity[units]

        def duration(milliseconds: Long): Quantity[units] =
          Quantity(milliseconds.toDouble/units.ratio())

        def milliseconds(duration: Quantity[units]): Long = (duration.normalize.value*1000).toLong

  extension (instant: Instant)
    @targetName("to")
    infix def ~ (that: Instant): Period = Period(instant, that)

    def tai: TaiInstant = LeapSeconds.tai(instant)

    infix def in (using RomanCalendar)(timezone: Timezone): Moment =
      val zonedTime = jt.Instant.ofEpochMilli(instant).nn.atZone(jt.ZoneId.of(timezone.name.s)).nn

      val date = zonedTime.getMonthValue.absolve match
        case Month(month) =>
          unsafely(Date(Year(zonedTime.getYear), month, Day(zonedTime.getDayOfMonth)))

      val time = (zonedTime.getHour, zonedTime.getMinute, zonedTime.getSecond).absolve match
        case (Base24(hour), Base60(minute), Base60(second)) => Clockface(hour, minute, second)

      Moment(date, time, timezone)

    def timestamp(using calendar: RomanCalendar, timezone: Timezone): Timestamp =
      in(timezone).timestamp

    def long: Long = instant

  extension (duration: into Duration)
    def from(instant: Instant): Period = Period(instant, Instant.plus.add(instant, duration))
