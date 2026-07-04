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
  // An `Instant` is an absolute point in time, stored as a `Long` whose interpretation — which
  // timeline it counts on (Unix/POSIX, TAI, …) — is the phantom `Transport` set by `over`. So
  // `Instant over Posix` and `Instant over Tai` are distinct types over the same grid; a
  // `Chronometry` given converts between them (through TAI). The unrefined `Instant` is abstract in
  // its chronometry and is rarely used directly.
  opaque type Instant = Long

  private def raw(instant: Instant): Long = instant

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

  object Instant:
    def Min[transport]: Instant over transport = Long.MinValue.asInstanceOf[Instant over transport]
    def Max[transport]: Instant over transport = Long.MaxValue.asInstanceOf[Instant over transport]

    // Construct in an explicit chronometry from its raw tick value (grounding code uses
    // `Instant.of[Posix]` with epoch milliseconds; `monotonic` uses `Instant.of[Monotonic]` with
    // nanoseconds).
    def of[transport](ticks: Long): Instant over transport =
      ticks.asInstanceOf[Instant over transport]

    // Construct in the import-selected default chronometry.
    def apply(millis: Long)(using ambient: Chronometry.Ambient): Instant over ambient.Transport =
      millis.asInstanceOf[Instant over ambient.Transport]

    inline given underlying: [transport] => Underlying[Instant over transport, Long] = !!


    // Generic interop: an instant on any timeline abstracts to/from an epoch `Long` (the `Long` is
    // tagged with the transport, not reinterpreted — conversion between timelines is `.over`).
    given generic: [transport]
    =>  (Instant over transport) is Abstractable & Instantiable across Instants to Long from Long =

      new Abstractable with Instantiable:
        type Self = Instant over transport
        type Origin = Long
        type Result = Long
        type Domain = Instants
        def apply(long: Long): Self = long.asInstanceOf[Self]
        def genericize(instant: Self): Long = raw(instant)


    inline given orderable: [transport] => (Instant over transport) is Orderable:
      inline def compare
        ( inline left:        Instant over transport,
          inline right:       Instant over transport,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        if left.long == right.long then !strict else (left.long < right.long) ^ greaterThan

    given ordering: [transport] => Ordering[Instant over transport] =
      Ordering.Long.asInstanceOf[Ordering[Instant over transport]]


    // Arithmetic converts a `Duration` (seconds) to the timeline's own ticks via its `Resolution`,
    // so it works whatever the resolution (milliseconds for `Posix`/`Tai`, nanoseconds for
    // `Monotonic`).
    private def ticks(seconds: Double, resolution: Long): Long =
      (seconds*1_000_000_000.0/resolution).toLong

    given plus: [transport, units <: Measure: Normalizable to Seconds[1]]
    =>  ( resolution: transport is Resolution )
    =>  (Instant over transport) is Addable by Quantity[units] to (Instant over transport) =
      Addable: (instant, duration) =>
        Instant.of(raw(instant) + ticks(duration.normalize.value, resolution.nanos))

    // The difference of two instants on the same timeline is a `Duration` in seconds; subtracting
    // across timelines is a type error (convert with `.over`).
    given minus: [transport]
    =>  ( resolution: transport is Resolution )
    =>  (Instant over transport) is Subtractable by (Instant over transport) to Duration =
      Subtractable: (left, right) => Quantity((raw(left) - raw(right))*resolution.nanos/1_000_000_000.0)

    given minusDuration: [transport, units <: Measure: Normalizable to Seconds[1]]
    =>  ( resolution: transport is Resolution )
    =>  (Instant over transport) is Subtractable by Quantity[units] to (Instant over transport) =
      Subtractable: (instant, duration) =>
        Instant.of(raw(instant) - ticks(duration.normalize.value, resolution.nanos))


  extension [transport](instant: Instant over transport)
    def long: Long = raw(instant)

    // Re-interpret this instant on another timeline, composing through TAI.
    def over[target](using from: transport is Chronometry, to: target is Chronometry)
    :   Instant over target =

      Instant.of(to.fromTai(from.toTai(raw(instant))))

    @targetName("to")
    infix def ~ (that: Instant over transport): Period[Instant over transport] =
      Period(instant, that)

    infix def in (using RomanCalendar, transport is Chronometry)(timezone: Timezone): Moment =
      val unix = instant.over[Posix].long
      val zone = jt.ZoneId.of(timezone.name.s).nn
      val zonedTime = jt.Instant.ofEpochMilli(unix).nn.atZone(zone).nn

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

    def timestamp(using RomanCalendar, transport is Chronometry, Timezone): Timestamp =
      in(summon[Timezone]).timestamp


  extension (duration: Duration)
    def from[transport](instant: Instant over transport)(using resolution: transport is Resolution)
    :   Period[Instant over transport] =

      val ticks = (duration.value*1_000_000_000.0/resolution.nanos).toLong
      Period(instant, Instant.of(raw(instant) + ticks))
