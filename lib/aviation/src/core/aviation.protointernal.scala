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
import hypotenuse.*
import prepositional.*
import quantitative.*
import rudiments.*
import symbolism.*

object protointernal:
  opaque type Instant = Long
  opaque type TaiInstant = Long

  object TaiInstant:
    def apply(taiMillis: Long): TaiInstant = taiMillis

    extension (tai: TaiInstant)
      def long: Long = tai

      // The inverse of `Instant.tai`: recover the Unix `Instant` from this `TaiInstant`. Only a
      // `Reversible` strategy (e.g. `smear`) can do this; `step` is not reversible.
      def instant(using strategy: LeapSeconds.Reversible): Instant = strategy.unix(tai)

    inline given underlying: Underlying[TaiInstant, Long] = !!


    given generic
    :   protointernal.TaiInstant is Abstractable & Instantiable across Instants to Long from Long =

      new Abstractable with Instantiable:
        type Self = protointernal.TaiInstant
        type Origin = Long
        type Result = Long
        type Domain = Instants
        def apply(long: Long): protointernal.TaiInstant = long
        def genericize(instant: protointernal.TaiInstant): Long = instant

  object InstantSubtractable:
    given instant: Instant is InstantSubtractable to Duration = new InstantSubtractable:
      type Self = Instant
      type Result = Duration

      def subtract(left: Instant, right: Instant): Duration = Quantity((left - right)/1000.0)


    given duration: [units <: Measure: Normalizable to Seconds[1]]
    =>  Quantity[units] is InstantSubtractable to Instant =

      new InstantSubtractable:
        type Self = Quantity[units]
        type Result = Instant

        def subtract(left: Instant, right: Quantity[units]): Instant =
          left - (right.normalize.value*1000.0).toLong

  trait InstantSubtractable extends Typeclass, Resultant:
    def subtract(left: Instant, right: Self): Result

  object Instant:
    final val Min: Instant = Long.MinValue
    final val Max: Instant = Long.MaxValue

    def apply[instant: Abstractable across Instants to Long](instant: instant): Instant =
      instant.generic

    inline given underlying: Underlying[Instant, Long] = !!


    given generic
    :   protointernal.Instant is Abstractable & Instantiable across Instants to Long from Long =

      new Abstractable with Instantiable:
        type Self = protointernal.Instant
        type Result = Long
        type Origin = Long
        type Domain = Instants
        def apply(long: Long): protointernal.Instant = long
        def genericize(instant: protointernal.Instant): Long = instant


    inline given orderable: Instant is Orderable:
      inline def compare
        ( inline left:        Instant,
          inline right:       Instant,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        if left.long == right.long then !strict else (left.long < right.long) ^ greaterThan

    given ordering: Ordering[Instant] = Ordering.Long


    given plus: [units <: Measure: Normalizable to Seconds[1]]
    =>  Instant is Addable by Quantity[units] to Instant = new Addable:

      type Self = Instant
      type Operand = Quantity[units]
      type Result = Instant

      def add(instant: Instant, duration: Quantity[units]): Instant =
        instant + (duration.normalize.value*1000.0).toLong


    given minus: [operand: InstantSubtractable]
    =>  Instant is Subtractable by operand to operand.Result =

      operand.subtract(_, _)

  type Duration = Quantity[Seconds[1]]

  object Duration:
    def apply(millis: Long): Duration = Quantity(millis/1000.0)


    given generic: [units <: Measure: Normalizable to Seconds[1]]
    =>  Quantity[units] is Abstractable & Instantiable across Durations from Long to Long =

      new Abstractable with Instantiable:
        type Self = Quantity[units]
        type Origin = Long
        type Result = Long
        type Domain = Durations

        def apply(nanoseconds: Long): Quantity[units] =
          Quantity(nanoseconds.toDouble/1_000_000_000.0/units.ratio())

        def genericize(duration: Quantity[units]): Long =
          (duration.normalize.value*1_000_000_000L).toLong


  extension (instant: into[Instant])
    @targetName("to")
    infix def ~ (that: into[Instant]): Period = Period(instant, that)

    def tai(using strategy: LeapSeconds.Strategy): TaiInstant = strategy.tai(instant)

    infix def in (using RomanCalendar)(timezone: Timezone): Moment =
      val zone = jt.ZoneId.of(timezone.name.s).nn
      val zonedTime = jt.Instant.ofEpochMilli(instant).nn.atZone(zone).nn

      val date = zonedTime.getMonthValue.absolve match
        case Month(month) =>
          unsafely(Date(Year(zonedTime.getYear), month, Day(zonedTime.getDayOfMonth)))

      val time = (zonedTime.getHour, zonedTime.getMinute, zonedTime.getSecond).absolve match
        case (Base24(hour), Base60(minute), Base60(second)) => Clockface(hour, minute, second)

      // During a fall-back overlap the same wall-clock time occurs twice; if this instant uses the
      // post-transition (later) offset, it is the second occurrence — record that so grounding the
      // `Moment` back to an `Instant` round-trips instead of silently picking the earlier offset.
      val transition = zone.getRules.nn.getTransition(zonedTime.toLocalDateTime.nn)

      val laterOffset =
        transition != null && transition.nn.isOverlap &&
          zonedTime.getOffset.nn.getTotalSeconds == transition.nn.getOffsetAfter.nn.getTotalSeconds

      Moment(date, time, timezone, if laterOffset then Occurrence.Second else Occurrence.First)

    def timestamp(using calendar: RomanCalendar, timezone: Timezone): Timestamp =
      in(timezone).timestamp

    def long: Long = instant


  extension (duration: into[Duration])
    def from(instant: into[Instant]): Period = Period(instant, Instant.plus.add(instant, duration))
