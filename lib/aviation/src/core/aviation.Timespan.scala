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
  given year: Int is Multiplicable by Year.type to (Timespan of Year.type) =
    (n, _) => Timespan(years = n).asInstanceOf[Timespan of Year.type]

  given month: Int is Multiplicable by Month.type to (Timespan of Month.type) =
    (n, _) => Timespan(months = n).asInstanceOf[Timespan of Month.type]

  given week: Int is Multiplicable by Week.type to (Timespan of Week.type) =
    (n, _) => Timespan(weeks = n).asInstanceOf[Timespan of Week.type]

  given day: Int is Multiplicable by Day.type to (Timespan of Day.type) =
    (n, _) => Timespan(days = n).asInstanceOf[Timespan of Day.type]

  given hour: Int is Multiplicable by Hour.type to (Timespan of Hour.type) =
    (n, _) => Timespan(hours = n).asInstanceOf[Timespan of Hour.type]

  given minute: Int is Multiplicable by Minute.type to (Timespan of Minute.type) =
    (n, _) => Timespan(minutes = n).asInstanceOf[Timespan of Minute.type]

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

case class Timespan
  ( years:   Int                 = 0,
    months:  Int                 = 0,
    weeks:   Int                 = 0,
    days:    Int                 = 0,
    hours:   Int                 = 0,
    minutes: Int                 = 0,
    seconds: Quantity[Seconds[1]] = Quantity(0.0) ):

  type Topic <: Radix
