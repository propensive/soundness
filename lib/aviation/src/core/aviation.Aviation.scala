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
┃    Soundness, version 0.44.0.                                                                    ┃
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
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import hypotenuse.*
import kaleidoscope.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import scala.quoted.*

import java.util as ju

object Aviation:
  opaque type Date = Int
  opaque type Year = Int
  opaque type Day = Int
  opaque type WorkingDays = Int
  opaque type Anniversary = Short

  extension (anniversary: Anniversary)
    inline def day: Day = anniversary%64
    inline def month: Month = Month.fromOrdinal(anniversary >> 6)


    def apply(year: Year)(using RomanCalendar)(using rounding: Anniversary.NonexistentLeapDay)
    : Date =

        safely(Date(year, month, day)).or(rounding.round(year))


  object WorkingDays:
    def apply(n: Int): WorkingDays = n

  extension (days: WorkingDays) def apply(): Int = days

  object Anniversary:
    trait NonexistentLeapDay:
      def round(year: Year): Date

    def apply(month: Month, day: Day): Anniversary = ((month.ordinal << 6) + day).toShort

    given showable: (endianness: Endianness, months: Months, separation: DateSeparation)
          => Anniversary is Showable =
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

  object Year:
    inline def apply(year: Int): Year = year
    given showable: Year is Showable = _.toString.tt
    given addable: Year is Addable by Int to Year = _ + _
    given subtractable: Year is Subtractable by Int to Year = _ - _

    given decodable: (Int is Decodable in Text) => Year is Decodable in Text = year =>
      Year(year.decode[Int])


    given orderable: Year is Orderable:
      inline def compare
                  (inline left:        Year,
                   inline right:       Year,
                   inline strict:      Boolean,
                   inline greaterThan: Boolean)
      : Boolean =

          if left == right then !strict else (left < right)^greaterThan


  object Day:
    inline def apply(day: Int): Day = day

    given decodable: (Int is Decodable in Text) => Day is Decodable in Text = day =>
      Day(day.decode[Int])

    given showable: Day is Showable = _.toString.tt

  def validTime(time: Expr[Double], pm: Boolean): Macro[Clockface] =
    import quotes.reflect.*

    time.asTerm match
      case Inlined(None, Nil, lit@Literal(DoubleConstant(d))) =>
        val hour = d.toInt
        val Base60(minutes: Base60) = ((d - hour) * 100 + 0.5).toInt: @unchecked

        if minutes >= 60 then halt(m"a time cannot have a minute value above 59", lit.pos)
        if hour < 0 then halt(m"a time cannot be negative", lit.pos)
        if hour > 12 then halt(m"a time cannot have an hour value above 12", lit.pos)

        val Base24(hours: Base24) = (hour + (if pm^(hour == 12) then 12 else 0))%24: @unchecked
        val length = lit.pos.endColumn - lit.pos.startColumn

        Position.ofMacroExpansion.sourceCode.get.tt match
          case r"[0-9][0-9]?\.[0-9][0-9][^0-9]*" =>

          case other =>
            halt(m"the time should have exactly two minutes digits", lit.pos)

        '{Clockface(${Expr[Base24](hours)}, ${Expr[Base60](minutes)}, 0)}

      case _ =>
        halt(m"expected a literal double value")

  object Date:
    erased given underlying: Underlying[Date, Int] = !!
    def of(day: Int): Date = day

    def apply(using calendar: Calendar)
         (year: calendar.Annual, month: calendar.Mensual, day: calendar.Diurnal)
    : Date raises TimeError =

        calendar.jdn(year, month, day)

    trait Format(val name: Text):
      type Issue: Communicable

    given showable: (Endianness, DateNumerics, DateSeparation, Years) => Date is Showable = date =>
      import DateNumerics.*, Years.*
      import textMetrics.uniform
      given calendar: RomanCalendar = calendars.gregorian

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
      import calendars.gregorian

      value.cut(t"-").to(List) match
        case As[Int](year) :: As[Int](month) :: As[Int](day) :: Nil =>
          Date(year, Month(month), day)

        case cnt =>
          raise(TimeError(_.Format(value, Iso8601, Prim)(Iso8601.Issue.Digit))) yet 2000-Jan-1


    given encodable: RomanCalendar => Date is Encodable in Text = date =>
      import hieroglyph.textMetrics.uniform
      List
       (date.year.toString.tt,
        date.month.numerical.toString.tt.pad(2, Rtl, '0'),
        date.day.toString.tt.pad(2, Rtl, '0'))

      . join(t"-")

    inline given orderable: Date is Orderable:
      inline def compare
                  (inline left:        Date,
                   inline right:       Date,
                   inline strict:      Boolean,
                   inline greaterThan: Boolean)
      : Boolean =

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

    given plus: (calendar: Calendar) => Date is Addable:
      type Result = Date
      type Operand = Timespan
      def add(date: Date, timespan: Timespan): Date = calendar.add(date, timespan)


  extension (date: Date)
    def day(using calendar: Calendar): calendar.Diurnal = calendar.diurnal(date)
    def month(using calendar: Calendar): calendar.Mensual = calendar.mensual(date)
    def year(using calendar: Calendar): calendar.Annual = calendar.annual(date)
    def weekday: Weekday = Weekday.fromOrdinal(jdn%7)
    def weekend(using hebdomad: Hebdomad): Boolean = weekday.weekend

    def anniversary: Anniversary =
      Anniversary(calendars.gregorian.mensual(date), calendars.gregorian.diurnal(date))

    def jdn: Int = date

    infix def at (time: Clockface)(using Calendar): Timestamp = Timestamp(date, time)

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
