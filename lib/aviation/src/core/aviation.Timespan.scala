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
import contextual.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import quantitative.*
import spectacular.*
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
    case Year          => Timespan(years = n).asInstanceOf[Timespan of radix]
    case _: MonthRadix => Timespan(months = n).asInstanceOf[Timespan of radix]
    case Week          => Timespan(weeks = n).asInstanceOf[Timespan of radix]
    case Day           => Timespan(days = n).asInstanceOf[Timespan of radix]
    case Hour          => Timespan(hours = n).asInstanceOf[Timespan of radix]
    case Minute        => Timespan(minutes = n).asInstanceOf[Timespan of radix]
    case _             => Timespan().asInstanceOf[Timespan of radix]

  // ISO-8601 duration text, e.g. `P1Y2M3DT4H5M6S`. Zero components are omitted; the all-zero span
  // is `PT0S`. Fractional seconds render with a decimal point.
  private def renderDuration(span: Timespan): Text =
    def part(value: Int, unit: Text): Text = if value == 0 then t"" else t"$value$unit"

    val seconds = span.seconds.value

    val secondsText =
      if seconds == 0.0 then t""
      else if seconds == seconds.toLong.toDouble then t"${seconds.toLong}S"
      else t"${seconds.toString.tt}S"

    val date =
      List(part(span.years, t"Y"), part(span.months, t"M"), part(span.weeks, t"W"),
          part(span.days, t"D")).join

    val time = List(part(span.hours, t"H"), part(span.minutes, t"M"), secondsText).join

    if date == t"" && time == t"" then t"PT0S"
    else if time == t"" then t"P$date"
    else t"P${date}T$time"

  // The ISO-8601 duration is the encoded (machine) form; a human-readable `Showable` is opt-in via
  // `import timespanFormats.relativeTimespan`.
  given encodable: Timespan is Encodable in Text = renderDuration(_)

  // Runtime parse of an ISO-8601 duration; shares `parseDuration` with the `dur"…"` interpolator.
  given decodable: Tactic[TimeError] => Timespan is Decodable in Text = text =>
    aviation.internal.parseDuration(text.s) match
      case Right((years, months, weeks, days, hours, minutes, seconds)) =>
        Timespan(years, months, weeks, days, hours, minutes, Quantity(seconds))

      case Left(_) =>
        abort(TimeError(_.Unknown(text, t"duration")))

  // Compile-time `dur"P1Y2M3DT4H5M6S"` literal, validated by the same parser.
  given interpolable: Timespan is Interpolable:
    inline def interpolate[parts <: Tuple, origins <: Tuple]
      ( inline insertions: Any* )
    :   Timespan =

      ${aviation.internal.durInterpolator[parts]('insertions)}

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

  // The shared year+month step: advance to the target year-and-month (floored division, so negative
  // spans behave) using the calendar's `monthsInYear`, then resolve any day-of-month overflow per
  // the contextual `Disambiguation`.
  private def anchorMonth(date: Date, addYears: Int, addMonths: Int)
    ( using calendar: Calendar, disambiguation: Disambiguation )
  :   Date =

    // Advance whole years (keeping the same month), then step the months one at a time, wrapping at
    // each year's own month count — so calendars with a variable number of months (Hebrew) work.
    var year = calendar.annual(date)() + addYears
    var ordinal = calendar.monthOrdinal(Year(year), calendar.mensual(date))
    var remaining = addMonths

    while remaining > 0 do
      ordinal += 1
      if ordinal >= calendar.monthsInYear(Year(year)) then { ordinal = 0; year += 1 }
      remaining -= 1

    while remaining < 0 do
      ordinal -= 1
      if ordinal < 0 then { year -= 1; ordinal = calendar.monthsInYear(Year(year)) - 1 }
      remaining += 1

    val month = calendar.monthOfOrdinal(Year(year), ordinal)
    val day = calendar.diurnal(date)()
    val length = calendar.daysInMonth(month, Year(year))

    if day <= length then unsafely(Date(Year(year), month, Day(day)))
    else disambiguation.resolve(Year(year), month, day)

  // A span with a year but no month advances whole years in any calendar (resolving Feb-29-style
  // overflow), then adds whole days.
  given dateYear: [topic <: Radix]
  =>  ( topic <:< Radix.Irregular, NotGiven[topic <:< MonthRadix], Calendar, Disambiguation )
  =>  Date is Addable by (Timespan of topic) to Date =
    (date, span) => anchorMonth(date, span.years, 0).addDays(span.days + span.weeks*7)

  // A span counted in some calendar's months: the in-scope `Calendar` must govern that month radix
  // (`topic <:< calendar.MonthUnit`), so a span's months can only be added to a date in the same
  // calendar — mixing calendars is a compile error.
  given dateMonths[topic <: Radix]
    ( using calendar: Calendar, ev: topic <:< calendar.MonthUnit, disambiguation: Disambiguation )
  :   (Date is Addable by (Timespan of topic) to Date) =

    (date, span) => anchorMonth(date, span.years, span.months).addDays(span.days + span.weeks*7)

  private def physicalSeconds(span: Timespan): Quantity[Seconds[1]] =
    val days = span.days.toLong + span.weeks.toLong*7
    val whole = days*86400 + span.hours.toLong*3600 + span.minutes.toLong*60
    Quantity[Seconds[1]](whole.toDouble) + span.seconds

  // A regular timespan (no Month/Year) is a fixed physical duration, so it adds to/subtracts from an
  // `Instant` (on any timeline); an irregular span cannot (it needs a calendar — add it to a date).
  given instantPlus: [transport, topic <: Radix] => NotGiven[topic <:< Radix.Irregular]
  =>  (Instant over transport) is Addable by (Timespan of topic) to (Instant over transport) =
    (instant, span) => instant + physicalSeconds(span)

  given instantMinus: [transport, topic <: Radix] => NotGiven[topic <:< Radix.Irregular]
  =>  (Instant over transport) is Subtractable by (Timespan of topic) to (Instant over transport) =
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
