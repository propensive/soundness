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

import scala.util.NotGiven

import anticipation.*
import contingency.*
import prepositional.*
import quantitative.*
import symbolism.*

// A `Timespan` is a duration expressed as a vector of radix counts (`Year`/`Month`/`Week`/`Day`/
// `Hour`/`Minute`) plus a physical `seconds` component (a `Quantity[Seconds[1]]`). Its `Topic`
// type-member records which radices it carries, as a type intersection — so `3*Month + 2*Week` has
// type `Timespan of (Month.type & Week.type)`. Combining timespans unions their radix sets, which
// is exactly type-intersection, so `+`/`-` intersect the operands' `Topic`.
//
// Radix counts are kept un-normalized (no carrying between radices): carrying months into years or
// days into months depends on a `Chronology` and an anchor date, so it happens only when a span is
// applied to a `Date`/`Timestamp`. A `Timespan` whose radices are all `Regular` (anything up to
// `Week`, but not `Month`/`Year`) is a definite physical duration, so it is `Abstractable` and
// `Instantiable across Durations` — usable anywhere a duration `Quantity` is.

object Timespan:
  // `n*Radix` builds a single-radix timespan (e.g. `3*Month : Timespan of Month.type`). The
  // `Int is Multiplicable by <radix>.type` givens live on each radix's companion object so they are
  // in implicit scope at the `n*Radix` call site; this builder backs them.
  def apply[radix <: Radix](unit: radix, n: Int): Timespan of radix = unit match
    case Year   => Timespan(years = n).asInstanceOf[Timespan of radix]
    case Month  => Timespan(months = n).asInstanceOf[Timespan of radix]
    case Week   => Timespan(weeks = n).asInstanceOf[Timespan of radix]
    case Day    => Timespan(days = n).asInstanceOf[Timespan of radix]
    case Hour   => Timespan(hours = n).asInstanceOf[Timespan of radix]
    case Minute => Timespan(minutes = n).asInstanceOf[Timespan of radix]
    case _      => Timespan().asInstanceOf[Timespan of radix]

  given addable: [left <: Radix, right <: Radix]
  =>  (Timespan of left) is Addable by (Timespan of right) to (Timespan of (left & right)) =
    (a, b) =>
      Timespan
        ( a.years + b.years,
          a.months + b.months,
          a.weeks + b.weeks,
          a.days + b.days,
          a.hours + b.hours,
          a.minutes + b.minutes,
          a.seconds + b.seconds )

      . asInstanceOf[Timespan of (left & right)]

  given subtractable: [left <: Radix, right <: Radix]
  =>  (Timespan of left) is Subtractable by (Timespan of right) to (Timespan of (left & right)) =
    (a, b) =>
      Timespan
        ( a.years - b.years,
          a.months - b.months,
          a.weeks - b.weeks,
          a.days - b.days,
          a.hours - b.hours,
          a.minutes - b.minutes,
          a.seconds - b.seconds )

      . asInstanceOf[Timespan of (left & right)]

  given multiplicable: [topic <: Radix]
  =>  (Timespan of topic) is Multiplicable by Int to (Timespan of topic) =
    (span, n) =>
      Timespan
        ( span.years*n,
          span.months*n,
          span.weeks*n,
          span.days*n,
          span.hours*n,
          span.minutes*n,
          Quantity(span.seconds.value*n) )

      . asInstanceOf[Timespan of topic]

  // Folding a physical seconds `Quantity` into a timespan adds `Seconds[1]` to its radix set.
  given quantityAddable: [topic <: Radix, units <: Measure: Normalizable to Seconds[1]]
  =>  (Timespan of topic) is Addable by Quantity[units] to (Timespan of (topic & Seconds[1])) =
    (span, quantity) =>
      val updated = span.copy(seconds = span.seconds + quantity.normalize)
      updated.asInstanceOf[Timespan of (topic & Seconds[1])]

  given quantitySubtractable: [topic <: Radix, units <: Measure: Normalizable to Seconds[1]]
  =>  (Timespan of topic) is Subtractable by Quantity[units] to (Timespan of (topic & Seconds[1])) =
    (span, quantity) =>
      val updated = span.copy(seconds = span.seconds - quantity.normalize)
      updated.asInstanceOf[Timespan of (topic & Seconds[1])]

  // A timespan with no irregular radices (no Month/Year) is a definite physical duration, so it
  // round-trips through `Durations` (nanoseconds) like a duration `Quantity`.
  given generic: [topic <: Radix] => NotGiven[topic <:< Radix.Irregular]
  =>  (Timespan of topic) is Abstractable & Instantiable across Durations from Long to Long =
    new Abstractable with Instantiable:
      type Self = Timespan of topic
      type Domain = Durations
      type Origin = Long
      type Result = Long

      def apply(nanoseconds: Long): Timespan of topic =
        Timespan(seconds = Quantity(nanoseconds/1_000_000_000.0)).asInstanceOf[Timespan of topic]

      def genericize(span: Timespan of topic): Long =
        val days = span.days.toLong + span.weeks.toLong*7
        val seconds = days*86400 + span.hours.toLong*3600 + span.minutes.toLong*60
        seconds*1_000_000_000L + (span.seconds.value*1_000_000_000.0).toLong

  // Adding a regular timespan (no Month/Year) to a date is a fixed number of whole days; sub-day
  // radices do not affect a date.
  given dateRegular: [topic <: Radix] => NotGiven[topic <:< Radix.Irregular]
  =>  Date is Addable by (Timespan of topic) to Date =
    (date, span) => date.addDays(span.days + span.weeks*7)

  // Adding a timespan with months/years steps over to the target year-and-month (floored), resolves
  // any day-of-month overflow per the contextual `Disambiguation`, then adds whole days.
  given dateCalendar: [topic <: Radix]
  =>  ( topic <:< Radix.Irregular, RomanCalendar, Disambiguation )
  =>  Date is Addable by (Timespan of topic) to Date =
    (date, span) =>
      val calendar = summon[RomanCalendar]
      val disambiguation = summon[Disambiguation]
      val months = span.years*12 + span.months
      val total = calendar.annual(date)()*12 + calendar.mensual(date).ordinal + months
      val year = Year(Math.floorDiv(total, 12))
      val month = Month.fromOrdinal(Math.floorMod(total, 12))
      val day = calendar.diurnal(date)()
      val length = calendar.daysInMonth(month, year)

      val anchor =
        if day <= length then unsafely(Date(year, month, Day(day)))
        else disambiguation.resolve(year, month, day)

      anchor.addDays(span.days + span.weeks*7)

  private def physicalSeconds(span: Timespan): Quantity[Seconds[1]] =
    val days = span.days.toLong + span.weeks.toLong*7
    val whole = days*86400 + span.hours.toLong*3600 + span.minutes.toLong*60
    Quantity[Seconds[1]](whole.toDouble) + span.seconds

  // A regular timespan (no Month/Year) is a fixed physical duration, so it adds to/subtracts from a
  // bare `Instant`; an irregular span cannot (it needs a calendar — add it to a date instead).
  given instantPlus: [topic <: Radix] => NotGiven[topic <:< Radix.Irregular]
  =>  Instant is Addable by (Timespan of topic) to Instant =
    (instant, span) => instant + physicalSeconds(span)

  given instantMinus: [topic <: Radix] => NotGiven[topic <:< Radix.Irregular]
  =>  Instant is Subtractable by (Timespan of topic) to Instant =
    (instant, span) => instant - physicalSeconds(span)

  // Adding a timespan to a (zoneless) timestamp applies the date/calendar part to the date (reusing
  // the `Date` instance, hence its calendar/disambiguation requirements) and rolls the sub-day part
  // through the clock, carrying whole days into the date.
  given timestampAdd: [topic <: Radix] => (dates: Date is Addable by (Timespan of topic) to Date)
  =>  Timestamp is Addable by (Timespan of topic) to Timestamp =
    (timestamp, span) =>
      val nanos = 1_000_000_000L
      val dayNanos = 86400L*nanos
      val time = timestamp.time
      val timeNanos = (time.hour*3600L + time.minute*60L + time.second)*nanos + time.nanos

      val spanNanos =
        (span.hours.toLong*3600 + span.minutes.toLong*60)*nanos +
          (span.seconds.value*nanos.toDouble).toLong

      val total = timeNanos + spanNanos
      val carry = Math.floorDiv(total, dayNanos).toInt
      val rem = Math.floorMod(total, dayNanos)
      val date = dates.add(timestamp.date, span).addDays(carry)

      val clockface =
        Clockface
          ( (rem/(3600L*nanos)).toInt.asInstanceOf[Base24],
            ((rem/(60L*nanos))%60).toInt.asInstanceOf[Base60],
            ((rem/nanos)%60).toInt.asInstanceOf[Base60],
            (rem%nanos).toInt )

      Timestamp(date, clockface)

  // Adding a timespan to a zoned `Moment` splits at the day boundary: the calendar/day part
  // advances the wall-clock (so a day "ignores" DST — same local time next day), while the sub-day
  // part is applied physically through the timezone (so hours/minutes/seconds "honour" DST).
  given momentAdd: [topic <: Radix]
  =>  ( Timestamp is Addable by (Timespan of topic) to Timestamp, RomanCalendar )
  =>  Moment is Addable by (Timespan of topic) to Moment =
    (moment, span) =>
      val timestamps = summon[Timestamp is Addable by (Timespan of topic) to Timestamp]
      val dateSpan = Timespan(span.years, span.months, span.weeks, span.days)
      val subSpan = Timespan(hours = span.hours, minutes = span.minutes, seconds = span.seconds)
      val wall = timestamps.add(moment.timestamp, dateSpan.asInstanceOf[Timespan of topic])
      val zoned = Moment(wall.date, wall.time, moment.timezone)

      (zoned.instant + physicalSeconds(subSpan)).in(moment.timezone)

case class Timespan
  ( years:   Int                 = 0,
    months:  Int                 = 0,
    weeks:   Int                 = 0,
    days:    Int                 = 0,
    hours:   Int                 = 0,
    minutes: Int                 = 0,
    seconds: Quantity[Seconds[1]] = Quantity(0.0) ):

  type Topic <: Radix
