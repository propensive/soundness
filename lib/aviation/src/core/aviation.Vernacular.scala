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
┃    Soundness, version 0.64.0.                                                                    ┃
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
import gossamer.*
import prepositional.*
import spectacular.*
import vacuous.*

// The time units a duration or frequency is built from, coarsest first.
enum TimeUnit:
  case Years, Months, Weeks, Days, Hours, Minutes, Seconds

object Vernacular:
  def components(span: Timespan): List[(Long, TimeUnit)] =
    List
     ( (span.years.toLong, TimeUnit.Years), (span.months.toLong, TimeUnit.Months),
       (span.weeks.toLong, TimeUnit.Weeks), (span.days.toLong, TimeUnit.Days),
       (span.hours.toLong, TimeUnit.Hours), (span.minutes.toLong, TimeUnit.Minutes),
       (span.seconds.value.toLong, TimeUnit.Seconds) )
    . filter(_(0) != 0)

  def unitOf(frequency: Frequency): TimeUnit = frequency match
    case Frequency.Secondly => TimeUnit.Seconds
    case Frequency.Minutely => TimeUnit.Minutes
    case Frequency.Hourly   => TimeUnit.Hours
    case Frequency.Daily    => TimeUnit.Days
    case Frequency.Weekly   => TimeUnit.Weeks
    case Frequency.Monthly  => TimeUnit.Months
    case Frequency.Yearly   => TimeUnit.Years

  val english: Vernacular = English
  val french: Vernacular = French
  val german: Vernacular = German
  val spanish: Vernacular = Spanish

  private object English extends Vernacular:
    private def word(unit: TimeUnit): (Text, Text) = unit match
      case TimeUnit.Years   => (t"year", t"years")
      case TimeUnit.Months  => (t"month", t"months")
      case TimeUnit.Weeks   => (t"week", t"weeks")
      case TimeUnit.Days    => (t"day", t"days")
      case TimeUnit.Hours   => (t"hour", t"hours")
      case TimeUnit.Minutes => (t"minute", t"minutes")
      case TimeUnit.Seconds => (t"second", t"seconds")

    private def ordinal(n: Int): Text = aviation.internal.englishOrdinal(n)
    private def monthDay(n: Int): Text =
      if n == -1 then t"last day" else if n < 0 then t"${ordinal(-n)}-to-last day" else ordinal(n)

    def conjunction: Text = t"and"
    def justNow: Text = t"just now"
    def future(body: Text): Text = t"in $body"
    def past(body: Text): Text = t"$body ago"
    def times(count: Int): Text = t", $count times"
    def from(start: Text): Text = t"from $start"

    def position(n: Int): Text =
      if n == -1 then t"last" else if n < 0 then t"${ordinal(-n)}-to-last" else ordinal(n)

    def units(count: Long, unit: TimeUnit): Text =
      t"$count ${if count == 1 then word(unit)(0) else word(unit)(1)}"

    def everyDuration(span: Timespan): Text = t"every ${conjoin(durations(span))}"

    def everyUnit(interval: Int, unit: TimeUnit): Text =
      if interval == 1 then t"every ${word(unit)(0)}" else t"every $interval ${word(unit)(1)}"

    def onDays(entries: List[(Optional[Int], Text)]): Text =
      val article = if entries.exists(_(0).present) then t"the " else t""
      t"on $article${conjoin(entries.map(dayPhrase))}"

    def onMonthDays(days: List[Int]): Text = t"on the ${conjoin(days.map(monthDay))}"

    def inMonths(names: List[Text], withDay: Boolean): Text =
      if withDay then t"of ${conjoin(names)}" else t"in ${conjoin(names)}"

    def takingPositions(positions: List[Int]): Text = t"taking the ${conjoin(positions.map(position))}"

  private object French extends Vernacular:
    private def word(unit: TimeUnit): (Text, Text, Boolean) = unit match
      case TimeUnit.Years   => (t"an", t"ans", true)
      case TimeUnit.Months  => (t"mois", t"mois", true)
      case TimeUnit.Weeks   => (t"semaine", t"semaines", false)
      case TimeUnit.Days    => (t"jour", t"jours", true)
      case TimeUnit.Hours   => (t"heure", t"heures", false)
      case TimeUnit.Minutes => (t"minute", t"minutes", false)
      case TimeUnit.Seconds => (t"seconde", t"secondes", false)

    private def ordinal(n: Int): Text = if n == 1 then t"1er" else t"${n}e"
    private def monthDay(n: Int): Text = if n == -1 then t"dernier jour" else t"$n"
    private def article(unit: TimeUnit): Text = if word(unit)(2) then t"tous les" else t"toutes les"

    def conjunction: Text = t"et"
    def justNow: Text = t"à l'instant"
    def future(body: Text): Text = t"dans $body"
    def past(body: Text): Text = t"il y a $body"
    def times(count: Int): Text = t", $count fois"
    def from(start: Text): Text = t"à partir du $start"
    def position(n: Int): Text = if n == -1 then t"dernier" else ordinal(n.abs)

    def units(count: Long, unit: TimeUnit): Text =
      t"$count ${if count == 1 then word(unit)(0) else word(unit)(1)}"

    def everyDuration(span: Timespan): Text =
      val first = Vernacular.components(span).headOption.map(_(1)).getOrElse(TimeUnit.Days)
      t"${article(first)} ${conjoin(durations(span))}"

    def everyUnit(interval: Int, unit: TimeUnit): Text =
      if interval == 1 then t"${article(unit)} ${word(unit)(1)}"
      else t"${article(unit)} $interval ${word(unit)(1)}"

    def onDays(entries: List[(Optional[Int], Text)]): Text = t"le ${conjoin(entries.map(dayPhrase))}"
    def onMonthDays(days: List[Int]): Text = t"le ${conjoin(days.map(monthDay))}"

    def inMonths(names: List[Text], withDay: Boolean): Text =
      if withDay then t"de ${conjoin(names)}" else t"en ${conjoin(names)}"

    def takingPositions(positions: List[Int]): Text =
      t"en prenant le ${conjoin(positions.map(position))}"

  private object German extends Vernacular:
    private def word(unit: TimeUnit): (Text, Text, Text) = unit match
      case TimeUnit.Years   => (t"Jahr", t"Jahre", t"jedes")
      case TimeUnit.Months  => (t"Monat", t"Monate", t"jeden")
      case TimeUnit.Weeks   => (t"Woche", t"Wochen", t"jede")
      case TimeUnit.Days    => (t"Tag", t"Tage", t"jeden")
      case TimeUnit.Hours   => (t"Stunde", t"Stunden", t"jede")
      case TimeUnit.Minutes => (t"Minute", t"Minuten", t"jede")
      case TimeUnit.Seconds => (t"Sekunde", t"Sekunden", t"jede")

    private def ordinal(n: Int): Text = t"$n."
    private def monthDay(n: Int): Text = if n == -1 then t"letzten Tag" else t"$n."
    private def setPos(n: Int): Text = if n == -1 then t"der letzte" else t"der ${ordinal(n.abs)}"

    def conjunction: Text = t"und"
    def justNow: Text = t"gerade eben"
    def future(body: Text): Text = t"in $body"
    def past(body: Text): Text = t"vor $body"
    def times(count: Int): Text = t", $count Mal"
    def from(start: Text): Text = t"ab $start"
    def position(n: Int): Text = if n == -1 then t"letzte" else ordinal(n.abs)

    def units(count: Long, unit: TimeUnit): Text =
      t"$count ${if count == 1 then word(unit)(0) else word(unit)(1)}"

    def everyDuration(span: Timespan): Text = Vernacular.components(span) match
      case List((1, unit)) => t"${word(unit)(2)} ${word(unit)(0)}"
      case parts           => t"alle ${conjoin(parts.map(quantity))}"

    def everyUnit(interval: Int, unit: TimeUnit): Text =
      if interval == 1 then t"${word(unit)(2)} ${word(unit)(0)}"
      else t"alle $interval ${word(unit)(1)}"

    def onDays(entries: List[(Optional[Int], Text)]): Text = t"am ${conjoin(entries.map(dayPhrase))}"
    def onMonthDays(days: List[Int]): Text = t"am ${conjoin(days.map(monthDay))}"
    def inMonths(names: List[Text], withDay: Boolean): Text = t"im ${conjoin(names)}"

    def takingPositions(positions: List[Int]): Text = t"davon ${conjoin(positions.map(setPos))}"

  private object Spanish extends Vernacular:
    private def word(unit: TimeUnit): (Text, Text) = unit match
      case TimeUnit.Years   => (t"año", t"años")
      case TimeUnit.Months  => (t"mes", t"meses")
      case TimeUnit.Weeks   => (t"semana", t"semanas")
      case TimeUnit.Days    => (t"día", t"días")
      case TimeUnit.Hours   => (t"hora", t"horas")
      case TimeUnit.Minutes => (t"minuto", t"minutos")
      case TimeUnit.Seconds => (t"segundo", t"segundos")

    private val ordinalWords: List[Text] =
      List(t"primer", t"segundo", t"tercer", t"cuarto", t"quinto", t"sexto", t"séptimo", t"octavo",
          t"noveno", t"décimo")

    private def ordinal(n: Int): Text =
      if n >= 1 && n <= ordinalWords.length then ordinalWords(n - 1) else t"$n.º"

    private def monthDay(n: Int): Text = if n == -1 then t"último día" else t"día $n"

    def conjunction: Text = t"y"
    def justNow: Text = t"justo ahora"
    def future(body: Text): Text = t"en $body"
    def past(body: Text): Text = t"hace $body"
    def times(count: Int): Text = t", $count veces"
    def from(start: Text): Text = t"desde $start"
    def position(n: Int): Text = if n == -1 then t"último" else ordinal(n.abs)

    def units(count: Long, unit: TimeUnit): Text =
      t"$count ${if count == 1 then word(unit)(0) else word(unit)(1)}"

    def everyDuration(span: Timespan): Text = t"cada ${conjoin(durations(span))}"

    def everyUnit(interval: Int, unit: TimeUnit): Text =
      if interval == 1 then t"cada ${word(unit)(0)}" else t"cada $interval ${word(unit)(1)}"

    def onDays(entries: List[(Optional[Int], Text)]): Text = t"el ${conjoin(entries.map(dayPhrase))}"
    def onMonthDays(days: List[Int]): Text = t"el ${conjoin(days.map(monthDay))}"

    def inMonths(names: List[Text], withDay: Boolean): Text =
      if withDay then t"de ${conjoin(names)}" else t"en ${conjoin(names)}"

    def takingPositions(positions: List[Int]): Text = t"tomando el ${conjoin(positions.map(position))}"

// A `Vernacular` supplies one language's vocabulary and grammar for describing recurrences and
// relative timespans in prose. The structural assembly (which clauses apply, in what order) is
// shared in the `final` methods; each language fills in the words and the grammatical hooks. These
// translations are a first pass intended for refinement by native speakers.
trait Vernacular:
  def units(count: Long, unit: TimeUnit): Text
  def conjunction: Text
  def position(n: Int): Text
  def justNow: Text
  def future(body: Text): Text
  def past(body: Text): Text
  def everyDuration(span: Timespan): Text
  def everyUnit(interval: Int, unit: TimeUnit): Text
  def times(count: Int): Text
  def from(start: Text): Text
  def onDays(entries: List[(Optional[Int], Text)]): Text
  def onMonthDays(days: List[Int]): Text
  def inMonths(names: List[Text], withDay: Boolean): Text
  def takingPositions(positions: List[Int]): Text

  protected final def conjoin(items: List[Text]): Text = items match
    case Nil        => t""
    case List(item) => item
    case other      => t"${other.init.join(t", ")} $conjunction ${other.last}"

  protected final def quantity(pair: (Long, TimeUnit)): Text = units(pair(0).abs, pair(1))
  protected final def durations(span: Timespan): List[Text] = Vernacular.components(span).map(quantity)

  protected final def dayPhrase(entry: (Optional[Int], Text)): Text =
    if entry(0).absent then entry(1) else t"${position(entry(0).vouch)} ${entry(1)}"

  final def relativeTimespan(span: Timespan): Text = Vernacular.components(span) match
    case Nil => justNow

    case parts =>
      val body = conjoin(parts.map(quantity))
      if parts.head(0) < 0 then past(body) else future(body)

  final def recurrence(period: Timespan, repetitions: Optional[Int], start: Text): Text =
    t"${everyDuration(period)}${repetitions.lay(t"")(times)} ${from(start)}"

  final def rrule(rule: Rrule[?])(using Months, Weekdays): Text =
    def dayEntry(entry: WeekdayOrdinal): (Optional[Int], Text) = (entry.ordinal, entry.weekday.show)
    def monthName(month: Month): Text = month.show

    val cadence = everyUnit(rule.interval, Vernacular.unitOf(rule.frequency))

    val onClause: Optional[Text] =
      if rule.byDay.nonEmpty then onDays(rule.byDay.map(dayEntry))
      else if rule.byMonthDay.nonEmpty then onMonthDays(rule.byMonthDay)
      else Unset

    val monthClause: Optional[Text] =
      if rule.byMonth.isEmpty then Unset else inMonths(rule.byMonth.map(monthName), onClause.present)

    val setPosClause: Optional[Text] =
      if rule.bySetPos.isEmpty then Unset else takingPositions(rule.bySetPos)

    val clauses =
      List(cadence) ++ onClause.lay(Nil)(List(_)) ++ monthClause.lay(Nil)(List(_))
        ++ setPosClause.lay(Nil)(List(_))

    t"${clauses.join(t" ")}${rule.count.lay(t"")(times)}"
