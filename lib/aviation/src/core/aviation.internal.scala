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

import scala.quoted.*

import abacist.*
import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gigantism.*
import gossamer.*
import hieroglyph.*
import hypotenuse.*
import kaleidoscope.*
import prepositional.*
import quantitative.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object internal:
  opaque type Date = Int
  opaque type Year = Int
  opaque type Day = Int
  opaque type WorkingDays = Int
  opaque type Anniversary = Short


  extension (anniversary: Anniversary)
    inline def day: Day = anniversary%64

    inline def month: Month = Month.fromOrdinal(anniversary >> 6)


    def apply(year: Year)(using RomanCalendar)(using rounding: Anniversary.NonexistentLeapDay)
    :   Date =

      safely(Date(year, month, day)).or(rounding.round(year))


  object WorkingDays:
    def apply(n: Int): WorkingDays = n

  extension (days: WorkingDays) def apply(): Int = days

  object Anniversary:
    trait NonexistentLeapDay:
      def round(year: Year): Date

    def apply(month: Month, day: Day): Anniversary = ((month.ordinal << 6) + day).toShort


    given showable: (endianness: Endianness, months: Months, separation: DateSeparation)
    =>  Anniversary is Showable =

      anniversary =>
        val month: Text = months.name(anniversary.month)

        endianness.match
          case Endianness.LittleEndian => t"${anniversary.day}${separation.separator}$month"
          case _                       => t"$month${separation.separator}${anniversary.day}"


  extension (year: Year)
    @targetName("yearValue")
    inline def apply(): Int = year


  extension (day: Day)
    @targetName("dayValue")
    inline def apply(): Int = day


  object Year extends Radix.Irregular:
    inline def apply(year: Int): Year = year

    given multiplicable: Int is Multiplicable by Year.type to (Timespan of Year.type) =
      (n, _) => Timespan(Year, n)

    given showable: Year is Showable = _.toString.tt
    given addable: Year is Addable by Int to Year = _ + _
    given subtractable: Year is Subtractable by Int to Year = _ - _

    given decodable: (Int is Decodable in Text) => Year is Decodable in Text = year =>
      Year(year.decode[Int])

    given orderable: Year is Orderable:
      inline def compare
        ( inline left:        Year,
          inline right:       Year,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        if left == right then !strict else (left < right)^greaterThan

  object Day extends Radix.Regular:
    inline def apply(day: Int): Day = day

    given multiplicable: Int is Multiplicable by Day.type to (Timespan of Day.type) =
      (n, _) => Timespan(Day, n)

    given decodable: (Int is Decodable in Text) => Day is Decodable in Text = day =>
      Day(day.decode[Int])

    given showable: Day is Showable = _.toString.tt

  def tzInterpolator[parts <: Tuple: Type](insertions: Expr[Seq[Any]]): Macro[Timezone] =
    import quotes.reflect.*

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    val parts = recur[parts](Nil)

    if parts.length != 1 then halt(m"a timezone literal cannot have substitutions")

    val name: String = parts.head

    try jt.ZoneId.of(name).nn
    catch case _: jt.zone.ZoneRulesException =>
      halt(m"${name.tt} is not a valid timezone identifier")

    '{unsafely(Timezone(${Expr(name)}.tt))}


  // A timestamp literal classified by its precision. The `ts"…"` macro maps each case to a
  // distinct result type: a bare year to `Year`, a year-month to `Monthstamp`, a date to
  // `Date`, a zoneless date-time to `Timestamp`, and a zoned date-time to `Moment`.
  private enum TsParsed:
    case YearOnly(year: Int)
    case MonthOnly(year: Int, month: Int)
    case DateOnly(jdn: Int)
    case TimeOnly(jdn: Int, hour: Int, minute: Int, second: Int, nanos: Int)
    case ZoneOnly(jdn: Int, hour: Int, minute: Int, second: Int, nanos: Int, zone: String)

  private val monthNames: List[String] =
    List("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  // Canonical ISO 8601: a four-digit year, then optional `-MM`, optional `-DD`, optional
  // `THH:MM` with optional `:SS` and fractional seconds, and an optional `Z`/offset zone.
  private val IsoPattern =
    ("""(\d{4})(?:-(\d{2})(?:-(\d{2})(?:[T ](\d{2}):(\d{2})""" +
      """(?::(\d{2})(?:[.,](\d{1,9}))?)?(Z|[+-]\d{2}:?\d{2}|[+-]\d{2})?)?)?)?""").r

  // RFC 1123, e.g. `Tue, 17 Jun 2024 14:30:45 GMT`.
  private val RfcPattern =
    ("""(Mon|Tue|Wed|Thu|Fri|Sat|Sun), (\d{2}) """ +
      """(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) """ +
      """(\d{4}) (\d{2}):(\d{2}):(\d{2}) GMT""").r

  // Validate a date against the given calendar (rejecting e.g. month 13 or 31 February) and return
  // its Julian day number, which the macro emits directly via `Date.julianDay`.
  private def jdnOf(calendar: RomanCalendar, year: Int, month: Int, day: Int)
  :   Either[Message, Int] =

    if month < 1 || month > 12 then Left(m"$month is not a valid month number")
    else
      val computed: Optional[Int] =
        safely(calendar.jdn(Year(year), Month.fromOrdinal(month - 1), Day(day)).jdn)

      val invalid = m"$year-$month-$day is not a valid date in the ${calendar.name} calendar"

      computed.lay(Left(invalid))(Right(_))

  private def checkTime(hour: Int, minute: Int, second: Int): Either[Message, Unit] =
    if hour > 23 then Left(m"$hour is not a valid hour")
    else if minute > 59 then Left(m"$minute is not a valid minute")
    else if second > 59 then Left(m"$second is not a valid number of seconds")
    else Right(())

  private def normalizeZone(zone: String): Either[Message, String] =
    val normalized =
      if zone == "Z" then "Z" else
        val sign = zone.charAt(0)
        val rest = zone.drop(1).filter(_ != ':')
        val hours = rest.take(2)
        val minutes = if rest.length > 2 then rest.substring(2) else "00"
        s"$sign$hours:$minutes"

    try
      jt.ZoneId.of(normalized).nn
      Right(normalized)
    catch case _: Exception => Left(m"$zone is not a valid timezone offset")

  private def isoTime
    ( jdn:    Int,
      hour:   Int,
      minute: Int,
      second: Int,
      nanos:  Int,
      zone:   String | Null )
  :   Either[Message, TsParsed] =

    checkTime(hour, minute, second).flatMap: _ =>
      zone match
        case null =>
          Right(TsParsed.TimeOnly(jdn, hour, minute, second, nanos))

        case zoneText: String =>
          normalizeZone(zoneText).map: zoneName =>
            TsParsed.ZoneOnly(jdn, hour, minute, second, nanos, zoneName)

  private def parseIso(text: String): Either[Message, TsParsed] = text match
    case IsoPattern(year, null, _, _, _, _, _, _) =>
      Right(TsParsed.YearOnly(year.nn.toInt))

    case IsoPattern(year, month, null, _, _, _, _, _) =>
      val monthValue = month.nn.toInt

      if monthValue < 1 || monthValue > 12 then Left(m"$monthValue is not a valid month number")
      else Right(TsParsed.MonthOnly(year.nn.toInt, monthValue))

    case IsoPattern(year, month, day, null, _, _, _, _) =>
      jdnOf(calendars.gregorianCalendar, year.nn.toInt, month.nn.toInt, day.nn.toInt)
      . map(TsParsed.DateOnly(_))

    case IsoPattern(year, month, day, hour, minute, second, frac, zone) =>
      val secondValue = if second == null then 0 else second.nn.toInt
      val nanos = if frac == null then 0 else (frac.nn + "000000000").take(9).toInt

      val parsed = jdnOf(calendars.gregorianCalendar, year.nn.toInt, month.nn.toInt, day.nn.toInt)

      parsed.flatMap: jdn =>
        isoTime(jdn, hour.nn.toInt, minute.nn.toInt, secondValue, nanos, zone)

    case _ =>
      Left(m"$text is not a valid ISO 8601 timestamp")

  private def parseRfc(text: String): Either[Message, TsParsed] = text match
    case RfcPattern(_, day, month, year, hour, minute, second) =>
      val hourValue = hour.nn.toInt
      val minuteValue = minute.nn.toInt
      val secondValue = second.nn.toInt
      val monthValue = monthNames.indexOf(month.nn) + 1

      val parsed = jdnOf(calendars.gregorianCalendar, year.nn.toInt, monthValue, day.nn.toInt)

      parsed.flatMap: jdn =>
        checkTime(hourValue, minuteValue, secondValue).map: _ =>
          TsParsed.ZoneOnly(jdn, hourValue, minuteValue, secondValue, 0, "GMT")

    case _ =>
      Left(m"$text is not a valid RFC 1123 timestamp")

  private def parseTimestamp(text: String): Either[Message, TsParsed] =
    if text.isEmpty then Left(m"a timestamp literal cannot be empty")
    else if text.head.isDigit then parseIso(text)
    else if text.head.isLetter then parseRfc(text)
    else Left(m"a timestamp must begin with a digit or a weekday name")

  def tsInterpolator[parts <: Tuple: Type](insertions: Expr[Seq[Any]])
  :   Macro[Year | Monthstamp | Date | Timestamp | Moment] =

    import quotes.reflect.*

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    val parts = recur[parts](Nil)

    if parts.length != 1 then halt(m"a timestamp literal cannot contain substitutions")

    parseTimestamp(parts.head) match
      case Left(error) =>
        halt(error)

      case Right(parsed) =>
        val result: Expr[Year | Monthstamp | Date | Timestamp | Moment] = parsed match
          case TsParsed.YearOnly(year) =>
            '{Year(${Expr(year)})}

          case TsParsed.MonthOnly(year, month) =>
            '{Monthstamp(Year(${Expr(year)}), Month.fromOrdinal(${Expr(month - 1)}))}

          case TsParsed.DateOnly(jdn) =>
            '{Date.julianDay(${Expr(jdn)})}

          case TsParsed.TimeOnly(jdn, hour, minute, second, nanos) =>
            '{Timestamp(Date.julianDay(${Expr(jdn)}), Clockface(Base24(${Expr(hour)}),
                Base60(${Expr(minute)}), Base60(${Expr(second)}), ${Expr(nanos)}))}

          case TsParsed.ZoneOnly(jdn, hour, minute, second, nanos, zone) =>
            '{Timestamp(Date.julianDay(${Expr(jdn)}), Clockface(Base24(${Expr(hour)}),
                Base60(${Expr(minute)}), Base60(${Expr(second)}), ${Expr(nanos)}))
                .in(unsafely(Timezone(${Expr(zone)}.tt)))}

        result


  // The inline `Monthstamp - day` operator splices here. When the year, month and day are all
  // compile-time literals (the `2012-Mar-8` form), validate the date against the Gregorian
  // calendar at compile time and emit its Julian day number directly. Otherwise emit the runtime
  // check, preserving today's behaviour. (Calendar-awareness is layered on in a later step.)
  def monthstampMinus(left: Expr[Monthstamp], right: Expr[Int]): Macro[Date] =
    import quotes.reflect.*

    // `underlyingArgument` beta-reduces the inlined operators and exposes the proxy-val bindings as
    // part of the tree; gather every `ValDef` so proxy references can be followed to their values.
    val leftTree = left.asTerm.underlyingArgument
    val rightTree = right.asTerm.underlyingArgument

    val collector = new TreeAccumulator[Map[Symbol, Term]]:
      def foldTree(env: Map[Symbol, Term], tree: Tree)(owner: Symbol): Map[Symbol, Term] =
        val env2 = tree match
          case valDef: ValDef => valDef.rhs match
            case Some(rhs) => env.updated(valDef.symbol, rhs)
            case None      => env

          case _ =>
            env

        foldOverTree(env2, tree)(owner)

    val owner = Symbol.spliceOwner
    val leftEnv = collector.foldTree(Map(), leftTree)(owner)
    val env: Map[Symbol, Term] = collector.foldTree(leftEnv, rightTree)(owner)

    // Matches the synthesized `asInstanceOf`/`$asInstanceOf$` casts inlining inserts.
    object Cast:
      def unapply(term: Term): Option[Term] = term match
        case TypeApply(Select(body, "asInstanceOf" | "$asInstanceOf$"), _) => Some(body)
        case _                                                             => None

    // Strip the wrappers inlining inserts (`Typed`, `Block`, `Inlined`, casts) and follow proxy
    // `Ident`s to their bound right-hand sides.
    def strip(term: Term): Term = term match
      case Inlined(_, _, body)                         => strip(body)
      case Typed(body, _)                              => strip(body)
      case Block(_, body)                              => strip(body)
      case Cast(body)                                  => strip(body)
      case ident: Ident if env.contains(ident.symbol)  => strip(env(ident.symbol))
      case _                                           => term

    // An `Int` constant, also peering through opaque wrappers like `Year(_)`/`Day(_)`.
    def constInt(term: Term): Optional[Int] = strip(term) match
      case Literal(IntConstant(value))                       => value
      case Apply(fn, List(arg)) if fn.symbol.name == "apply" => constInt(arg)
      case _                                                 => Unset

    // The zero-based ordinal of a `Month` enum-case reference (e.g. `Mar`).
    def monthOrdinal(term: Term): Optional[Int] =
      monthNames.indexOf(strip(term).symbol.name) match
        case -1      => Unset
        case ordinal => ordinal

    // The calendar contextually in scope, if any. Used at runtime for the deferred check, and at
    // compile time when we recognise it as one whose validation we can run here.
    val summoned: Option[Expr[RomanCalendar]] = Expr.summon[RomanCalendar]

    // The runtime check, used whenever the operands aren't all compile-time literals; defaults to
    // the Gregorian calendar when nothing is in scope, preserving today's behaviour.
    def runtime: Expr[Date] =
      val calendar = summoned.getOrElse('{calendars.gregorianCalendar})
      '{unsafely($calendar.jdn($left.year, $left.month, Day($right)))}

    // Identify a contextual calendar as one of this module's calendar givens (matched by leaf name,
    // since it may be reached through the `soundness` re-export rather than its `aviation` origin;
    // the `.calendars.` guard stops an unrelated user calendar being misidentified), or `Unset` if
    // we don't recognise it, in which case compile-time validation is skipped.
    def recognise(expr: Expr[RomanCalendar]): Optional[RomanCalendar] =
      val symbol = strip(expr.asTerm).symbol

      if !symbol.fullName.contains(".calendars.") then Unset else symbol.name match
        case "gregorianCalendar" => calendars.gregorianCalendar
        case "julianCalendar"    => calendars.julianCalendar
        case "papalCutover"      => calendars.papalCutover
        case "britishCutover"    => calendars.britishCutover
        case _                   => Unset

    // The calendar to validate against at compile time: the recognised contextual one, or the
    // Gregorian default when none is in scope.
    val staticCalendar: Optional[RomanCalendar] =
      summoned.map(recognise).getOrElse(calendars.gregorianCalendar)

    // Validate a literal `Monthstamp(Year(y), month)` minus a literal `day` at compile time. Note
    // the plain control flow (no `Optional.lay`/`let` around the quoted trees): wrapping inline
    // `Optional` combinators around quotes crashes the compiler in `pickleQuotes`.
    strip(leftTree) match
      case Apply(fn, List(yearArg, monthArg))
      if fn.symbol.name == "apply" || fn.symbol.name == "<init>" =>
        val year = constInt(yearArg)
        val month = monthOrdinal(monthArg)
        val day = constInt(rightTree)

        val literal = year.present && month.present && day.present

        if !literal then runtime else staticCalendar match
          case calendar: RomanCalendar =>
            jdnOf(calendar, year.vouch, month.vouch + 1, day.vouch) match
              case Right(jdn)  => '{Date.julianDay(${Expr(jdn)})}
              case Left(error) => halt(error)

          case _ =>
            runtime

      case _ =>
        runtime


  def validTime(time: Expr[Double], pm: Boolean): Macro[Clockface] =
    import quotes.reflect.*

    time.asTerm match
      case Inlined(None, Nil, lit@Literal(DoubleConstant(d))) =>
        val hour = d.toInt
        val Base60(minutes: Base60) = ((d - hour) * 100 + 0.5).toInt: @unchecked

        if minutes >= 60 then halt(93, m"a time cannot have a minute value above 59", lit.pos)
        if hour < 0 then halt(955, m"a time cannot be negative", lit.pos)
        if hour > 12 then halt(81, m"a time cannot have an hour value above 12", lit.pos)

        val Base24(hours: Base24) = (hour + (if pm^(hour == 12) then 12 else 0))%24: @unchecked
        val length = lit.pos.endColumn - lit.pos.startColumn

        Position.ofMacroExpansion.sourceCode.get.tt match
          case r"[0-9][0-9]?\.[0-9][0-9][^0-9]*" =>

          case other =>
            halt(582, m"the time should have exactly two minutes digits", lit.pos)

        '{Clockface(${Expr[Base24](hours)}, ${Expr[Base60](minutes)}, 0)}

      case _ =>
        halt(735, m"expected a literal double value")

  object Date:
    inline given underlying: Underlying[Date, Int] = !!
    def julianDay(day: Int): Date = day

    def apply(using calendar: Calendar)
      ( year: calendar.Annual, month: calendar.Mensual, day: calendar.Diurnal )
    :   Date raises TimeError =

      calendar.jdn(year, month, day)


    trait Format(val name: Text):
      type Issue: Communicable

    given subtractable: Date is Subtractable by Date to Quanta[Days[1]] = (end, start) =>
      (Quanta(end - start): Quanta[Days[1]])

    given subtractable2: Date is Subtractable by Quanta[Days[1]] to Date = (end, start) =>
      end - start[Days]

    given addable: Date is Addable by Quanta[Days[1]] to Date = (left, right) =>
      left + right[Days]

    given addable2: Quanta[Days[1]] is Addable by Date to Date = (left, right) =>
      left[Days] + right

    given showable: (Endianness, DateNumerics, DateSeparation, Years) => Date is Showable = date =>
      import DateNumerics.*, Years.*
      import textMetrics.uniformMetric

      given calendar: RomanCalendar = calendars.gregorianCalendar

      def pad(n: Int): Text = (n%100).show.pad(2, Rtl, '0')

      val year: Text = summon[Years] match
        case TwoDigitYear => pad(date.year)
        case FullYear     => date.year.show

      val month: Text = summon[DateNumerics] match
        case FixedWidth    => pad(date.month.numerical)
        case VariableWidth => date.month.numerical.show

      val day: Text = summon[DateNumerics] match
        case FixedWidth    => pad(date.day)
        case VariableWidth => date.day.show

      summon[Endianness].match
        case Endianness.LittleEndian => List(day, month, year)
        case Endianness.MiddleEndian => List(month, day, year)
        case Endianness.BigEndian    => List(year, month, day)

      . join(summon[DateSeparation].separator)

    given decoder: Tactic[TimeError] => Date is Decodable in Text = value =>
      import calendars.gregorianCalendar

      value.cut(t"-").to(List) match
        case As[Int](year) :: As[Int](month) :: As[Int](day) :: Nil => Date(year, Month(month), day)

        case cnt =>
          abort(TimeError(_.Format(value, Iso8601, Prim)(Iso8601.Issue.Digit)))

    given encodable: RomanCalendar => Date is Encodable in Text = date =>
      import hieroglyph.textMetrics.uniformMetric

      List
        ( date.year.toString.tt,
          date.month.numerical.toString.tt.pad(2, Rtl, '0'),
          date.day.toString.tt.pad(2, Rtl, '0') )

      . join(t"-")

    inline given orderable: Date is Orderable:
      inline def compare
        ( inline left:        Date,
          inline right:       Date,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        if left == right then !strict else (left < right)^greaterThan

    given ordering: Ordering[Date] = Ordering.Int

    given plus2: Holidays => (hebdomad: Hebdomad) => Date is Addable:
      type Operand = WorkingDays
      type Result = Date

      def add(date: Date, days: WorkingDays): Date =
        def recur(current: Date, count: Int): Date =
          if count == 0 then
            if current.weekend || summon[Holidays].holiday(current).present
            then recur(current + 1, 0)
            else current
          else
            val next = current.jdn + count
            val holidays = summon[Holidays].between(current, next)
            val weekends = Weekday.all.to(List).filter(_.weekend)
            val weekendDays = weekends.map(Weekday.count(current, next, _)).sum
            val weekdayHolidays = holidays.filter(!_.date.weekend).length
            val skipped = weekdayHolidays + weekendDays
            recur(next, skipped)

        recur(date, days)

  extension (date: Date)
    def day(using calendar: Calendar): calendar.Diurnal = calendar.diurnal(date)

    def month(using calendar: Calendar): calendar.Mensual = calendar.mensual(date)
    def year(using calendar: Calendar): calendar.Annual = calendar.annual(date)
    def weekday: Weekday = Weekday.fromOrdinal(jdn%7)
    def weekend(using hebdomad: Hebdomad): Boolean = weekday.weekend

    def anniversary: Anniversary =
      Anniversary
        ( calendars.gregorianCalendar.mensual(date),
          calendars.gregorianCalendar.diurnal(date) )

    def jdn: Int = date

    def monthstamp(using calendar: RomanCalendar): Monthstamp =
      Monthstamp(calendar.annual(date), calendar.mensual(date))

    def yearDay(using calendar: Calendar): Int =
      date - calendar.zerothDayOfYear(calendar.annual(date))

    def addDays(count: Int): Date = date + count

    @targetName("gt")
    infix def > (right: Date): Boolean = date > right

    @targetName("lt")
    infix def < (right: Date): Boolean = date < right

    @targetName("lte")
    infix def <= (right: Date): Boolean = date <= right

    @targetName("gte")
    infix def >= (right: Date): Boolean = date >= right
