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
import denominative.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object Rfc1123 extends Date.Format(t"RFC 1123"):
  given Issue is Communicable =
    case Issue.DayName(days*)     => m"expected a day name (${days.map(_.show).join(t", ")})"
    case Issue.MonthName(months*) => m"expected a month name (${months.map(_.show).join(t", ")})"
    case Issue.Expect(char: Char) => m"expected $char"
    case Issue.Digit              => m"expected a digit"

  enum Issue:
    case DayName(days: Weekday*)
    case MonthName(months: Month*)
    case Expect(char: Char)
    case Digit

  def parse(text: Text): Instant over Posix raises TimeError =
    import Rfc1123.Issue.*
    import Weekday.*

    type Self = Instant
    type Form = Text

    var index: Ordinal = Prim

    def fail(issue: Rfc1123.Issue): Unit = raise(TimeError(_.Format(text, Rfc1123, index)(issue)))
    def focus: Char = text.at(index).or('\u0000')
    def next(): Char = (index += 1) yet focus
    def expect(char: Char): Unit = if next() != char then fail(Expect(char))

    focus match
      case 'M' => if next() != 'o' || next() != 'n' then fail(DayName(Mon))

      case 'T' =>
        next() match
          case 'u' => if next() != 'e' then fail(DayName(Tue))
          case 'h' => if next() != 'u' then fail(DayName(Thu))
          case _   => fail(DayName(Tue, Thu))

      case 'W' =>
        if next() != 'e' || next() != 'd' then fail(DayName(Wed))

      case 'F' =>
        if next() != 'r' || next() != 'i' then fail(DayName(Fri))

      case 'S' =>
        next() match
          case 'a' => if next() != 't' then fail(DayName(Sat))
          case 'u' => if next() != 'n' then fail(DayName(Sun))
          case _   => fail(DayName(Sat, Sun))

      case _ =>
        fail(DayName(Mon, Tue, Wed, Thu, Fri, Sat, Sun))

    expect(',')
    expect(' ')

    val day: Int =
      (if next() < '0' || focus > '9' then fail(Digit) yet 0 else (focus - '0')*10) +
        (if next() < '0' || focus > '9' then fail(Digit) yet 1 else focus - '0')

    expect(' ')

    val month: Month = next() match
      case 'A' =>
        next() match
          case 'p' => if next() != 'r' then fail(MonthName(Apr)) yet Apr else Apr
          case 'u' => if next() != 'g' then fail(MonthName(Aug)) yet Aug else Aug
          case _   => fail(MonthName(Apr, Aug)) yet Apr

      case 'D' =>
        if next() != 'e' || next() != 'c' then fail(MonthName(Dec)) yet Dec else Dec

      case 'F' =>
        if next() != 'e' || next() != 'b' then fail(MonthName(Feb)) yet Feb else Feb

      case 'J' =>
        next() match
          case 'a' => if next() != 'n' then fail(MonthName(Jan)) yet Jan else Jan

          case 'u' => next() match
            case 'l' => Jul
            case 'n' => Jun
            case _   => fail(MonthName(Jun, Jul)) yet Jul

          case _   => fail(MonthName(Jan, Jun, Jul)) yet Jan

      case 'M' =>
        if next() != 'a' then fail(MonthName(Mar, May)) yet Mar else next() match
          case 'r' => Mar
          case 'y' => May
          case _   => fail(MonthName(Mar, May)) yet Mar

      case 'N' =>
        if next() != 'o' || next() != 'v' then fail(MonthName(Nov)) yet Nov else Nov

      case 'O' =>
        if next() != 'c' || next() != 't' then fail(MonthName(Oct)) yet Oct else Oct

      case 'S' =>
        if next() != 'e' || next() != 'p' then fail(MonthName(Sep)) yet Sep else Sep

      case _ =>
        fail(MonthName(Month.all*)) yet Jan

    expect(' ')

    val year: Int =
      (if next() < '0' || focus > '9' then fail(Digit) yet 2 else (focus - '0')*1000) +
        (if next() < '0' || focus > '9' then fail(Digit) yet 0 else (focus - '0')*100) +
        (if next() < '0' || focus > '9' then fail(Digit) yet 0 else (focus - '0')*10) +
        (if next() < '0' || focus > '9' then fail(Digit) yet 0 else focus - '0')

    expect(' ')

    val hour: Int =
      (if next() < '0' || focus > '9' then fail(Digit) yet 0 else (focus - '0')*10) +
        (if next() < '0' || focus > '9' then fail(Digit) yet 0 else focus - '0')

    expect(':')

    val minute: Int =
      (if next() < '0' || focus > '9' then fail(Digit) yet 0 else (focus - '0')*10) +
        (if next() < '0' || focus > '9' then fail(Digit) yet 1 else focus - '0')

    expect(':')

    val second: Int =
      (if next() < '0' || focus > '9' then fail(Digit) yet 0 else (focus - '0')*10) +
        (if next() < '0' || focus > '9' then fail(Digit) yet 1 else focus - '0')

    expect(' ')
    expect('G')
    expect('M')
    expect('T')

    import calendars.gregorianCalendar

    val date = Date(Year(year), month, Day(day))
    val time = Clockface(Base24(hour), Base60(minute), Base60(second))

    Timestamp(date, time).in(tz"GMT").instant
