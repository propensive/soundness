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

import anticipation.*
import contingency.*
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
// engine currently expands `Daily`…`Yearly` with `byMonth`, `byMonthDay`, `byDay` and `bySetPos`;
// sub-day frequencies and `byYearDay`/`byWeekNo`/`byHour`/… are not yet expanded.
object Rrule:
  given dateRecurrent: (RomanCalendar, Ordering[Date])
  =>  ( Rrule[Date] is Recurrent { type Topic = Date } ) =
    rule => bounded(dates(rule.start, rule), rule.until, rule.count)

  given timestampRecurrent: (RomanCalendar, Ordering[Timestamp])
  =>  ( Rrule[Timestamp] is Recurrent { type Topic = Timestamp } ) =
    rule =>
      val stamps = dates(rule.start.date, rule).map(Timestamp(_, rule.start.time))
      bounded(stamps, rule.until, rule.count)

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
        part(rule.byMonthDay.nonEmpty, t"BYMONTHDAY=${rule.byMonthDay.map(_.show).join(t",")}") ++
        part(rule.byDay.nonEmpty, t"BYDAY=${rule.byDay.map(renderDay).join(t",")}") ++
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

  // ── plain-English description ────────────────────────────────────────────────────────────────
  // `.encode` is the RFC 5545 wire form; `.show` describes the rule in English, e.g. "every month
  // on the 3rd Monday" or "every year on the 4th Thursday of November".

  private def unitName(frequency: Frequency, plural: Boolean): Text = frequency match
    case Frequency.Secondly => if plural then t"seconds" else t"second"
    case Frequency.Minutely => if plural then t"minutes" else t"minute"
    case Frequency.Hourly   => if plural then t"hours" else t"hour"
    case Frequency.Daily    => if plural then t"days" else t"day"
    case Frequency.Weekly   => if plural then t"weeks" else t"week"
    case Frequency.Monthly  => if plural then t"months" else t"month"
    case Frequency.Yearly   => if plural then t"years" else t"year"

  private def ordinalWord(n: Int): Text =
    if n == -1 then t"last"
    else if n < 0 then t"${aviation.internal.englishOrdinal(-n)}-to-last"
    else aviation.internal.englishOrdinal(n)

  given showable: [point] => (Months, Weekdays) => Rrule[point] is Showable = rule =>
    val join = aviation.internal.joinAnd

    val cadence =
      if rule.interval == 1 then t"every ${unitName(rule.frequency, false)}"
      else t"every ${rule.interval} ${unitName(rule.frequency, true)}"

    val byDayText: Optional[Text] =
      if rule.byDay.isEmpty then Unset else
        val hasOrdinal = rule.byDay.exists(_.ordinal.present)

        val entries = rule.byDay.map: entry =>
          if entry.ordinal.absent then entry.weekday.show
          else t"${ordinalWord(entry.ordinal.vouch)} ${entry.weekday.show}"

        t"on ${if hasOrdinal then t"the " else t""}${join(entries)}"

    val byMonthDayText: Optional[Text] =
      if rule.byMonthDay.isEmpty then Unset
      else t"on the ${join(rule.byMonthDay.map(monthDayWord))}"

    val onText: Optional[Text] = if rule.byDay.nonEmpty then byDayText else byMonthDayText

    val monthText: Optional[Text] =
      if rule.byMonth.isEmpty then Unset else
        val names = join(rule.byMonth.map(_.show))
        if onText.present then t"of $names" else t"in $names"

    val setPosText: Optional[Text] =
      if rule.bySetPos.isEmpty then Unset else t"taking the ${join(rule.bySetPos.map(ordinalWord))}"

    val countText = if rule.count.absent then t"" else t", ${rule.count.vouch} times"

    val clauses =
      List(cadence) ++ onText.lay(Nil)(List(_)) ++ monthText.lay(Nil)(List(_)) ++
        setPosText.lay(Nil)(List(_))

    t"${clauses.join(t" ")}$countText"

  private def monthDayWord(day: Int): Text =
    if day == -1 then t"last day"
    else if day < 0 then t"${aviation.internal.englishOrdinal(-day)}-to-last day"
    else aviation.internal.englishOrdinal(day)

  // ── the expansion engine (Gregorian) ────────────────────────────────────────────────────────

  private def yearOf(date: Date)(using calendar: RomanCalendar): Int = date.year(using calendar)()
  private def monthOf(date: Date)(using calendar: RomanCalendar): Int = date.month.numerical
  private def dayOf(date: Date)(using calendar: RomanCalendar): Int = date.day(using calendar)()
  private def list(date: Optional[Date]): List[Date] = date.lay(Nil)(List(_))

  // The ascending stream of dates matching the rule, starting at or after `start` (COUNT/UNTIL are
  // applied later, on the point stream). Each period is expanded independently and concatenated;
  // since periods are ascending and disjoint, the result is ascending.
  private def dates(start: Date, rule: Rrule[?])(using calendar: RomanCalendar): LazyList[Date] =
    val raw: LazyList[Date] = rule.frequency match
      case Frequency.Yearly =>
        LazyList.iterate(yearOf(start))(_ + rule.interval).flatMap: year =>
          yearMonths(year, start, rule).flatMap: month =>
            setPos(expandMonth(year, month, start, rule), rule.bySetPos)

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
    bySetPos:   List[Int]            = Nil,
    weekStart:  Weekday              = Weekday.Mon )
