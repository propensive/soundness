                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                    ╭───╮                                         ┃
┃  ╭─────────╮                                       │   │                                         ┃
┃  │   ╭─────╯╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮  ┃
┃  │   ╰─────╮│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯  ┃
┃  ╰─────╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ├╌╯╌─╯╰─╌ ╰───╮╰─╌ ╰───╮  ┃
┃  ╭─────╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╌   │╭───╌   │  ┃
┃  ╰─────────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯  ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0. © Copyright 2023-25 Jon Pretty, Propensive OÜ.                     ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        http://www.apache.org/licenses/LICENSE-2.0                                                ┃
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
import fulminate.*
import gossamer.*
import hypotenuse.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*

import scala.quoted.*

import java.util as ju

object Aviation:
  opaque type Date = Int

  def validTime(time: Expr[Double], pm: Boolean)(using Quotes): Expr[Clockface] =
    import quotes.reflect.*

    time.asTerm match
      case Inlined(None, Nil, lit@Literal(DoubleConstant(d))) =>
        val hour = d.toInt
        val minutes = ((d - hour) * 100 + 0.5).toInt

        if minutes >= 60 then halt(m"a time cannot have a minute value above 59", lit.pos)
        if hour < 0 then halt(m"a time cannot be negative", lit.pos)
        if hour > 12 then halt(m"a time cannot have an hour value above 12", lit.pos)

        val h: Base24 = (hour + (if pm then 12 else 0)).asInstanceOf[Base24]
        val length = lit.pos.endColumn - lit.pos.startColumn

        if (hour < 10 && length != 4) || (hour >= 10 && length != 5) then
          if length == 0
          then warn(m"time is unchecked because range positions are not available")
          else halt(m"the time should have exactly two minutes digits", lit.pos)

        val m: Base60 = minutes.asInstanceOf[Base60]
        '{Clockface(${Expr[Base24](h)}, ${Expr[Base60](m)}, 0)}

      case _ =>
        halt(m"expected a literal double value")

  object Date:
    erased given underlying: Underlying[Date, Int] = ###
    def of(day: Int): Date = day

    def apply(using cal: Calendar)(year: cal.Year, month: cal.Month, day: cal.Day)
    :     Date raises DateError =
      cal.julianDay(year, month, day)

    given show: Date is Showable = d =>
      given RomanCalendar = calendars.gregorian
      t"${d.day.toString.show}-${d.month.show}-${d.year.toString.show}"

    given decoder: Tactic[DateError] => Date is Decodable in Text = parse(_)

    given encodable: RomanCalendar => Date is Encodable in Text = date =>
      import hieroglyph.textMetrics.uniform
      List
       (date.year.toString.tt,
        date.month.numerical.toString.tt.pad(2, Rtl, '0'),
        date.day.toString.tt.pad(2, Rtl, '0'))

      . join(t"-")

    inline given orderable: Date is Orderable:
      inline def compare
         (inline left:     Date,
          inline right:      Date,
          inline strict:     Boolean,
          inline greaterThan: Boolean)
      :     Boolean =
        if left == right then !strict else (left < right)^greaterThan

    given ordering: Ordering[Date] = Ordering.Int

    given plus: (calendar: Calendar) => Date is Addable:
      type Result = Date
      type Operand = Timespan
      def add(date: Date, timespan: Timespan): Date = calendar.add(date, timespan)

    def parse(value: Text)(using Tactic[DateError]): Date = value.cut(t"-").to(List) match
      // FIXME: This compiles successfully, but never seems to match
      //case As[Int](year) :: As[Int](month) :: As[Int](day) :: Nil =>
      case y :: m :: d :: Nil =>
        try
          import calendars.gregorian
          Date(y.s.toInt, MonthName(m.s.toInt), d.s.toInt)
        catch
          case error: NumberFormatException =>
            raise(DateError(value), Date(using calendars.gregorian)(2000, MonthName(1), 1))

          case error: ju.NoSuchElementException =>
            raise(DateError(value), Date(using calendars.gregorian)(2000, MonthName(1), 1))

      case cnt =>
        raise(DateError(value), Date(using calendars.gregorian)(2000, MonthName(1), 1))

  extension (date: Date)
    def day(using calendar: Calendar): calendar.Day = calendar.getDay(date)
    def month(using calendar: Calendar): calendar.Month = calendar.getMonth(date)
    def year(using calendar: Calendar): calendar.Year = calendar.getYear(date)

    def yearDay(using calendar: Calendar): Int =
      date - calendar.zerothDayOfYear(calendar.getYear(date))

    def julianDay: Int = date
    def addDays(count: Int): Date = date + count

    infix def at (time: Clockface)(using Calendar): Timestamp = Timestamp(date, time)

    @targetName("gt")
    infix def > (right: Date): Boolean = date > right

    @targetName("lt")
    infix def < (right: Date): Boolean = date < right

    @targetName("lte")
    infix def <= (right: Date): Boolean = date <= right

    @targetName("gte")
    infix def >= (right: Date): Boolean = date >= right
