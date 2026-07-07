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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import anticipation.*
import contingency.*
import cosmopolite.{Locale, en, fr, de, es}
import distillate.*
import gossamer.*
import prepositional.*
import spectacular.*
import vacuous.*

// One `BYDAY` entry: a weekday, optionally with an ordinal — `3MO` (3rd Monday), `-1FR` (last
// Friday), or a bare `TU` (every Tuesday in the period). The ordinal is meaningful only under
// `Monthly`/`Yearly`; under `Weekly`/`Daily` a bare weekday acts as a filter.
case class WeekdayOrdinal(weekday: Weekday, ordinal: Optional[Int] = Unset)

// An iCalendar recurrence rule (RFC 5545 `RRULE`), pairing the rule with its start (`DTSTART`).
// Occurrences expand each `frequency`×`interval` period and apply the `by*` anchors; invalid dates
// (e.g. Feb 30) are skipped and the day-of-month re-anchors to `start`, so `Monthly` from Jan 31
// yields Jan 31, Mar 31, May 31, … (RFC behaviour, deliberately unlike `Recurrence`'s drift). The
// engine expands every `frequency` (`Secondly`…`Yearly`) with all the `by*` anchors — `byMonth`,
// `byWeekNo`, `byYearDay`, `byMonthDay`, `byDay`, `byHour`, `byMinute`, `bySecond`, `bySetPos`. A
// `Rrule[Date]` yields dates, a `Rrule[Timestamp]` yields zoneless date-times (expanded by
// `byHour`/…), and a `Rrule[Moment]` grounds each in the start's timezone.
object Rrule:
  given dateRecurrent: (RomanCalendar, Ordering[Date])
  =>  ( Rrule[Date] is Recurrent { type Topic = Date } ) =
    rule => bounded(dates(rule.start, rule), rule.until, rule.count)

  given timestampRecurrent: (RomanCalendar, Ordering[Timestamp])
  =>  ( Rrule[Timestamp] is Recurrent { type Topic = Timestamp } ) =
    rule => bounded(civil(rule.start, rule), rule.until, rule.count)

  given momentRecurrent: (RomanCalendar, Ordering[Moment])
  =>  ( Rrule[Moment] is Recurrent { type Topic = Moment } ) =
    rule =>
      val zone = rule.start.timezone
      val starting = Timestamp(rule.start.date, rule.start.time)
      bounded(civil(starting, rule).map(_.in(zone)), rule.until, rule.count)

  // Apply COUNT (take) and UNTIL (inclusive upper bound) to a generated, ascending stream.
  private def bounded[point](stream: LazyList[point], until: Optional[point], count: Optional[Int])
    ( using order: Ordering[point] )
  :   LazyList[point] =

    val capped = until.lay(stream): limit =>
      stream.takeWhile(!order.gt(_, limit))

    count.lay(capped)(capped.take)

  // ── RFC 5545 text form ───────────────────────────────────────────────────────────────────────
  // The rule is serialised on its own (the `DTSTART`/`start` is separate in iCalendar), e.g.
  // `FREQ=MONTHLY;INTERVAL=2;BYDAY=3MO;COUNT=10`. `parse` reattaches a supplied `start`.

  private val weekdayCodes: List[Text] = List(t"MO", t"TU", t"WE", t"TH", t"FR", t"SA", t"SU")

  private def code(weekday: Weekday): Text = weekdayCodes(weekday.ordinal)

  private def renderDay(entry: WeekdayOrdinal): Text =
    entry.ordinal.lay(code(entry.weekday)): ordinal =>
      t"$ordinal${code(entry.weekday)}"

  given encodable: [point: Encodable in Text] => Rrule[point] is Encodable in Text = rule =>
    def part(condition: Boolean, text: => Text): List[Text] = if condition then List(text) else Nil

    val parts =
      part(true, t"FREQ=${rule.frequency.toString.tt.upper}") ++
        part(rule.interval != 1, t"INTERVAL=${rule.interval}") ++
        part(rule.count.present, t"COUNT=${rule.count.vouch}") ++
        part(rule.until.present, t"UNTIL=${rule.until.vouch.encode}") ++
        part(rule.byMonth.nonEmpty, t"BYMONTH=${rule.byMonth.map(_.numerical.show).join(t",")}") ++
        part(rule.byWeekNo.nonEmpty, t"BYWEEKNO=${rule.byWeekNo.map(_.show).join(t",")}") ++
        part(rule.byYearDay.nonEmpty, t"BYYEARDAY=${rule.byYearDay.map(_.show).join(t",")}") ++
        part(rule.byMonthDay.nonEmpty, t"BYMONTHDAY=${rule.byMonthDay.map(_.show).join(t",")}") ++
        part(rule.byDay.nonEmpty, t"BYDAY=${rule.byDay.map(renderDay).join(t",")}") ++
        part(rule.byHour.nonEmpty, t"BYHOUR=${rule.byHour.map(_.show).join(t",")}") ++
        part(rule.byMinute.nonEmpty, t"BYMINUTE=${rule.byMinute.map(_.show).join(t",")}") ++
        part(rule.bySecond.nonEmpty, t"BYSECOND=${rule.bySecond.map(_.show).join(t",")}") ++
        part(rule.bySetPos.nonEmpty, t"BYSETPOS=${rule.bySetPos.map(_.show).join(t",")}") ++
        part(rule.weekStart != Weekday.Mon, t"WKST=${code(rule.weekStart)}")

    parts.join(t";")

  def parse[point: Decodable in Text](text: Text, start: point)(using Tactic[RruleError])
  :   Rrule[point] =

    val fields: Map[Text, Text] =
      text.cut(t";").flatMap: pair =>
        pair.cut(t"=") match
          case List(key, value) => List(key.upper -> value)
          case _                => Nil

      . to(Map)

    def field(key: Text): Optional[Text] = fields.get(key).getOrElse(Unset)

    Rrule
      ( start,
        field(t"FREQ").lay(abort(RruleError(text)))(frequencyOf(_, text)),
        field(t"INTERVAL").lay(1)(intOf(_, text)),
        field(t"COUNT").lay(Unset)(intOf(_, text)),
        field(t"UNTIL").lay(Unset)(_.decode[point]),
        field(t"BYMONTH").lay(Nil)(ints(_, text).map { n => Month.fromOrdinal(n - 1) }),
        field(t"BYMONTHDAY").lay(Nil)(ints(_, text)),
        field(t"BYDAY").lay(Nil) { value => value.cut(t",").to(List).map(dayOf(_, text)) },
        field(t"BYYEARDAY").lay(Nil)(ints(_, text)),
        field(t"BYWEEKNO").lay(Nil)(ints(_, text)),
        field(t"BYHOUR").lay(Nil)(ints(_, text)),
        field(t"BYMINUTE").lay(Nil)(ints(_, text)),
        field(t"BYSECOND").lay(Nil)(ints(_, text)),
        field(t"BYSETPOS").lay(Nil)(ints(_, text)),
        field(t"WKST").lay(Weekday.Mon)(weekdayOf(_, text)) )

  private def frequencyOf(text: Text, context: Text)(using Tactic[RruleError]): Frequency =
    text.upper match
      case t"SECONDLY" => Frequency.Secondly
      case t"MINUTELY" => Frequency.Minutely
      case t"HOURLY"   => Frequency.Hourly
      case t"DAILY"    => Frequency.Daily
      case t"WEEKLY"   => Frequency.Weekly
      case t"MONTHLY"  => Frequency.Monthly
      case t"YEARLY"   => Frequency.Yearly
      case _           => abort(RruleError(context))

  private def intOf(text: Text, context: Text)(using Tactic[RruleError]): Int =
    val string = text.s
    val body = if string.startsWith("-") || string.startsWith("+") then string.drop(1) else string
    if body.nonEmpty && body.forall(_.isDigit) then string.toInt else abort(RruleError(context))

  private def ints(text: Text, context: Text)(using Tactic[RruleError]): List[Int] =
    text.cut(t",").map(intOf(_, context))

  private def weekdayOf(text: Text, context: Text)(using Tactic[RruleError]): Weekday =
    weekdayCodes.indexOf(text.upper) match
      case -1    => abort(RruleError(context))
      case index => Weekday.fromOrdinal(index)

  private def dayOf(text: Text, context: Text)(using Tactic[RruleError]): WeekdayOrdinal =
    val string = text.s
    if string.length < 2 then abort(RruleError(context))
    val weekday = weekdayOf(string.takeRight(2).tt, context)
    val prefix = string.dropRight(2)
    WeekdayOrdinal(weekday, if prefix.isEmpty then Unset else intOf(prefix.tt, context))

  // ── natural-language description ─────────────────────────────────────────────────────────────
  // `.encode` is the RFC 5545 wire form; `.show` describes the rule in prose, e.g. "every month on
  // the 3rd Monday". Each language is a `Vernacular`, gated on its `Locale`.

  given englishShowable: [point] => (Months, Weekdays, Locale[en]) => Rrule[point] is Showable =
    Vernacular.english.rrule(_)

  given frenchShowable: [point] => (Months, Weekdays, Locale[fr]) => Rrule[point] is Showable =
    Vernacular.french.rrule(_)

  given germanShowable: [point] => (Months, Weekdays, Locale[de]) => Rrule[point] is Showable =
    Vernacular.german.rrule(_)

  given spanishShowable: [point] => (Months, Weekdays, Locale[es]) => Rrule[point] is Showable =
    Vernacular.spanish.rrule(_)

  // ── the expansion engine (Gregorian) ────────────────────────────────────────────────────────

  private def yearOf(date: Date)(using calendar: RomanCalendar): Int = date.year(using calendar)()
  private def monthOf(date: Date)(using calendar: RomanCalendar): Int = date.month.numerical
  private def dayOf(date: Date)(using calendar: RomanCalendar): Int = date.day(using calendar)()
  private def list(date: Optional[Date]): List[Date] = date.lay(Nil)(List(_))

  // The ascending stream of zoneless date-times. A sub-day frequency steps the clock and filters by
  // the `by*` limits; a date-level frequency expands the date stream and then the time-of-day via
  // `byHour`/`byMinute`/`bySecond` (defaulting to the start's time).
  private def civil(start: Timestamp, rule: Rrule[?])(using RomanCalendar): LazyList[Timestamp] =
    rule.frequency match
      case Frequency.Hourly | Frequency.Minutely | Frequency.Secondly =>
        val step = rule.frequency match
          case Frequency.Hourly   => rule.interval*3600
          case Frequency.Minutely => rule.interval*60
          case _                  => rule.interval

        LazyList.iterate(start)(addSeconds(_, step)).filter(subDayMatch(_, rule))

      case _ =>
        dates(start.date, rule).flatMap(expandTimes(_, start, rule)).dropWhile(_ < start)

  // Expand a date into the times-of-day the rule selects (the `byHour`/`byMinute`/`bySecond` cross
  // product, each defaulting to the start's component).
  private def expandTimes(date: Date, start: Timestamp, rule: Rrule[?]): List[Timestamp] =
    val hours = if rule.byHour.nonEmpty then rule.byHour.sorted else List(start.hour)
    val minutes = if rule.byMinute.nonEmpty then rule.byMinute.sorted else List(start.minute)
    val seconds = if rule.bySecond.nonEmpty then rule.bySecond.sorted else List(start.second)

    for
      hour   <- hours
      minute <- minutes
      second <- seconds
    yield Timestamp(date, Clockface(Base24(hour), Base60(minute), Base60(second)))

  // Step a zoneless timestamp by `n` wall-clock seconds, carrying whole days into the date.
  private def addSeconds(timestamp: Timestamp, n: Int)(using RomanCalendar): Timestamp =
    val total = timestamp.hour*3600 + timestamp.minute*60 + timestamp.second + n
    val seconds = Math.floorMod(total, 86400)
    val nanos = timestamp.time.nanos
    val time = Clockface(Base24(seconds/3600), Base60(seconds%3600/60), Base60(seconds%60), nanos)

    Timestamp(timestamp.date.addDays(Math.floorDiv(total, 86400)), time)

  private def subDayMatch(timestamp: Timestamp, rule: Rrule[?])(using RomanCalendar): Boolean =
    dailyMatch(timestamp.date, rule) &&
      (rule.byHour.isEmpty || rule.byHour.contains(timestamp.hour)) &&
      (rule.byMinute.isEmpty || rule.byMinute.contains(timestamp.minute)) &&
      (rule.bySecond.isEmpty || rule.bySecond.contains(timestamp.second))

  // The date that is the `n`th day of `year` (negative counts back from the end).
  private def yearDay(year: Int, n: Int)(using calendar: RomanCalendar): Optional[Date] =
    val total = calendar.daysInYear(Year(year))
    val index = if n > 0 then n else total + n + 1
    if index < 1 || index > total then Unset else date(year, 1, 1).let(_.addDays(index - 1))

  // The `byYearDay` dates within a year, filtered by `byMonth`.
  private def yearDayDates(year: Int, rule: Rrule[?])(using RomanCalendar): List[Date] =
    rule.byYearDay.flatMap { day => list(yearDay(year, day)) }.filter(monthAllowed(_, rule))

  // The `byWeekNo` dates within a week-year (ISO weeks, Monday-based): for each selected week number
  // (negatives count back from the last week), the `byDay` weekdays — or the start's weekday — of
  // that week, filtered by `byMonth`.
  private def weekNoDates(year: Int, start: Date, rule: Rrule[?])(using RomanCalendar): List[Date] =
    val count = WeekDate.weekOfYear(unsafely(Date(Year(year), Month.Dec, Day(28))))
    val weekdays = if rule.byDay.nonEmpty then rule.byDay.map(_.weekday) else List(start.weekday)

    val weeks =
      rule.byWeekNo.map(week => if week > 0 then week else count + week + 1)
        .filter(week => week >= 1 && week <= count)

    val dates =
      for
        week    <- weeks
        weekday <- weekdays
      yield safely(WeekDate(Year(year), week, weekday))

    dates.flatMap(list).filter(monthAllowed(_, rule))

  // The ascending stream of dates matching the rule, starting at or after `start` (COUNT/UNTIL are
  // applied later, on the point stream). Each period is expanded independently and concatenated;
  // since periods are ascending and disjoint, the result is ascending.
  private def dates(start: Date, rule: Rrule[?])(using calendar: RomanCalendar): LazyList[Date] =
    val raw: LazyList[Date] = rule.frequency match
      case Frequency.Yearly =>
        LazyList.iterate(yearOf(start))(_ + rule.interval).flatMap: year =>
          val candidates =
            if rule.byYearDay.nonEmpty then yearDayDates(year, rule)
            else if rule.byWeekNo.nonEmpty then weekNoDates(year, start, rule)
            else yearMonths(year, start, rule).flatMap(expandMonth(year, _, start, rule))

          setPos(candidates.distinct.sortBy(_.jdn), rule.bySetPos)

      case Frequency.Monthly =>
        months(start, rule.interval).flatMap: (year, month) =>
          setPos(expandMonth(year, month, start, rule), rule.bySetPos)

      case Frequency.Weekly =>
        weeks(start, rule).flatMap: weekStart =>
          setPos(expandWeek(weekStart, start, rule), rule.bySetPos)

      case Frequency.Daily =>
        LazyList.iterate(start)(_.addDays(rule.interval)).filter(dailyMatch(_, rule))

      case _ =>
        LazyList.empty // sub-day frequencies not yet expanded

    raw.dropWhile(_.jdn < start.jdn)

  // The months to expand within a `Yearly` period: the listed `byMonth`s, or every month if a
  // day-level rule is present, or else the start's own month.
  private def yearMonths(year: Int, start: Date, rule: Rrule[?])(using RomanCalendar): List[Int] =
    if rule.byMonth.nonEmpty then rule.byMonth.map(_.numerical).sorted
    else if rule.byDay.nonEmpty || rule.byMonthDay.nonEmpty then (1 to 12).to(List)
    else List(monthOf(start))

  // The ascending (year, month) periods for `Monthly`, stepping `interval` months from the start.
  private def months(start: Date, interval: Int)(using RomanCalendar): LazyList[(Int, Int)] =
    val first = yearOf(start)*12 + (monthOf(start) - 1)

    LazyList.iterate(first)(_ + interval).map: n =>
      (n/12, n%12 + 1)

  // The ascending week-start dates for `Weekly`, aligned to `weekStart`, stepping `interval` weeks.
  private def weeks(start: Date, rule: Rrule[?]): LazyList[Date] =
    val offset = (start.weekday.ordinal - rule.weekStart.ordinal + 7)%7
    LazyList.iterate(start.addDays(-offset))(_.addDays(7*rule.interval))

  // The sorted, deduplicated candidate dates within one month, per the day-level rules.
  private def expandMonth(year: Int, month: Int, start: Date, rule: Rrule[?])(using RomanCalendar)
  :   List[Date] =

    val byDayDates: Optional[List[Date]] =
      if rule.byDay.isEmpty then Unset else rule.byDay.flatMap: entry =>
        if entry.ordinal.absent then weekdaysOfMonth(year, month, entry.weekday)
        else list(nthWeekday(year, month, entry.weekday, entry.ordinal.vouch))

    val byMonthDayDates: Optional[List[Date]] =
      if rule.byMonthDay.isEmpty then Unset else rule.byMonthDay.flatMap: day =>
        list(monthDay(year, month, day))

    val candidates =
      if byDayDates.present && byMonthDayDates.present
      then byDayDates.vouch.filter(byMonthDayDates.vouch.contains)
      else if byDayDates.present then byDayDates.vouch
      else if byMonthDayDates.present then byMonthDayDates.vouch
      else list(monthDay(year, month, dayOf(start)))

    candidates.distinct.sortBy(_.jdn)

  // The candidate dates within one week (the 7 days from `weekStart`), per `byDay` (or the start's
  // weekday), filtered by `byMonth`.
  private def expandWeek(weekStart: Date, start: Date, rule: Rrule[?])(using RomanCalendar)
  :   List[Date] =

    val weekdays = if rule.byDay.nonEmpty then rule.byDay.map(_.weekday) else List(start.weekday)

    (0 to 6).to(List).map(weekStart.addDays(_)).filter: date =>
      weekdays.contains(date.weekday) && monthAllowed(date, rule)

  private def dailyMatch(date: Date, rule: Rrule[?])(using RomanCalendar): Boolean =
    monthAllowed(date, rule) &&
      (rule.byMonthDay.isEmpty || rule.byMonthDay.exists(monthDayMatches(date, _))) &&
      (rule.byDay.isEmpty || rule.byDay.map(_.weekday).contains(date.weekday))

  private def monthAllowed(date: Date, rule: Rrule[?])(using RomanCalendar): Boolean =
    rule.byMonth.isEmpty || rule.byMonth.map(_.numerical).contains(monthOf(date))

  // ── calendar helpers ─────────────────────────────────────────────────────────────────────────

  private def daysInMonth(year: Int, month: Int)(using calendar: RomanCalendar): Int =
    calendar.daysInMonth(Month.fromOrdinal(month - 1), Year(year))

  private def date(year: Int, month: Int, day: Int)(using RomanCalendar): Optional[Date] =
    safely(Date(Year(year), Month.fromOrdinal(month - 1), Day(day)))

  // A `byMonthDay` value resolved against a month: positive from the 1st, negative from the end.
  private def monthDay(year: Int, month: Int, day: Int)(using RomanCalendar): Optional[Date] =
    if day > 0 then date(year, month, day)
    else date(year, month, daysInMonth(year, month) + day + 1)

  private def monthDayMatches(value: Date, day: Int)(using RomanCalendar): Boolean =
    val days = daysInMonth(yearOf(value), monthOf(value))
    dayOf(value) == (if day > 0 then day else days + day + 1)

  private def weekdaysOfMonth(year: Int, month: Int, weekday: Weekday)(using RomanCalendar)
  :   List[Date] =

    list(date(year, month, 1)).flatMap: first =>
      val offset = (weekday.ordinal - first.weekday.ordinal + 7)%7
      LazyList.iterate(first.addDays(offset))(_.addDays(7))
        .takeWhile(monthOf(_) == month).to(List)

  // The `ordinal`-th `weekday` of the month (positive from the start, negative from the end).
  private def nthWeekday(year: Int, month: Int, weekday: Weekday, ordinal: Int)(using RomanCalendar)
  :   Optional[Date] =

    val all = weekdaysOfMonth(year, month, weekday)
    val index = if ordinal > 0 then ordinal - 1 else all.length + ordinal
    if index >= 0 && index < all.length then all(index) else Unset

  // `BYSETPOS`: from each period's expanded set, keep the listed positions (1-based; negatives from
  // the end).
  private def setPos(candidates: List[Date], positions: List[Int]): List[Date] =
    if positions.isEmpty then candidates else
      val count = candidates.length

      val chosen = positions.flatMap: position =>
        val index = if position > 0 then position - 1 else count + position
        if index >= 0 && index < count then List(candidates(index)) else Nil

      chosen.distinct.sortBy(_.jdn)

case class Rrule[point]
  ( start:      point,
    frequency:  Frequency,
    interval:   Int                  = 1,
    count:      Optional[Int]        = Unset,
    until:      Optional[point]      = Unset,
    byMonth:    List[Month]          = Nil,
    byMonthDay: List[Int]            = Nil,
    byDay:      List[WeekdayOrdinal] = Nil,
    byYearDay:  List[Int]            = Nil,
    byWeekNo:   List[Int]            = Nil,
    byHour:     List[Int]            = Nil,
    byMinute:   List[Int]            = Nil,
    bySecond:   List[Int]            = Nil,
    bySetPos:   List[Int]            = Nil,
    weekStart:  Weekday              = Weekday.Mon )
