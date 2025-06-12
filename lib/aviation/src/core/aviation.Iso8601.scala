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
┃    Soundness, version 0.34.0.                                                                    ┃
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
import quantitative.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object Iso8601 extends Date.Format(t"ISO 8601"):
  given Issue is Communicable =
    case Issue.Expect(char  ) => m"$char was expected"
    case Issue.Digit          => m"a digit was expected"
    case Issue.DigitOrDash    => m"a digit or ${'-'} was expected"
    case Issue.DigitOrDashOrW => m"a digit, ${'-'} or ${'W'} was expected"
    case Issue.DigitOrColon   => m"a digit or ${':'} was expected"
    case Issue.Timezone       => m"a timezone or ${'Z'} was expected"

  enum Issue:
    case Expect(char: Char)
    case Digit
    case DigitOrDash
    case DigitOrDashOrW
    case DigitOrColon
    case Timezone

  import Issue.*

  def parse(text: Text): Instant raises TimeError =
    import calendars.gregorian
    given Timezone = tz"UTC"
    var index: Ordinal = Prim
    def fail(issue: Iso8601.Issue): Unit = raise(TimeError(_.Format(text, Iso8601, index)(issue)))
    def focus: Char = text.at(index).or('\u0000')
    def next(): Char = (index += 1) yet focus
    def digit: Boolean = focus >= '0' && focus <= '9'

    def number(digits: Int, value: Int = focus - '0'): Int = if digits == 1 then value else
      next()
      if !digit then fail(Digit) yet 0
      else number(digits - 1, value*10 + (focus - '0'))

    def fraction(value: Double = 0.0, part: Double = 0.1): Double =
      next()
      if !digit then if part == 0.1 then fail(Digit) yet value else value
      else fraction(value + (focus - '0')*part, part/10.0)

    if !digit then fail(Digit)
    val year: Year = Year(number(4))

    val date: Date = next() match
      case '-' =>
        next()
        if !digit then fail(Digit) yet today() else
          val month: Month = Month(number(2))

          next() match
            case '\u0000' =>
              Date(year, month, Day(1))

            case '-' =>
              next()
              if !digit then fail(Digit) yet today() else Date(year, month, Day(number(2)))

            case _ =>
              fail(DigitOrDash) yet today()

      case 'W' =>
        2000-Jan-1

      case d if digit =>
        val month: Month = Month(number(2))
        next() match
          case d if digit => Date(year, month, Day(number(2)))
          case _          => fail(Digit) yet 2000-Jan-1

      case _ =>
        fail(Digit) yet 2000-Jan-1


    val instant: Instant = next() match
      case '\u0000' => date.at(Clockface(Base24(0), Base60(0), Base60(0))).instant
      case 'T' =>
        val hour = next() yet number(2)

        next() match
          case ':' =>
            val minute = next() yet number(2)

            next() match
              case '.' | ',' =>
                date.at(Clockface(Base24(hour), Base60(minute), Base60(0))).instant
                + fraction()*Minute

              case ':' =>
                val second = next() yet number(2)

                next() match
                  case '.' | ',' =>
                    date.at(Clockface(Base24(hour), Base60(minute), Base60(second))).instant
                    + fraction()*Second

                  case _ =>
                    date.at(Clockface(Base24(hour), Base60(minute), Base60(second))).instant

              case d if digit =>
                val second = number(2)
                next()
                date.at(Clockface(Base24(hour), Base60(minute), Base60(second))).instant

              case _ =>
                date.at(Clockface(Base24(hour), Base60(minute), Base60(0))).instant

          case d if digit =>
            val minute = number(2)

            next() match
              case '.' | ',' =>
                date.at(Clockface(Base24(hour), Base60(minute), Base60(0))).instant
                + fraction()*Minute

              case d if digit =>
                val second = number(2)
                next() match
                  case '.' | ',' =>
                    date.at(Clockface(Base24(hour), Base60(minute), Base60(second))).instant
                    + fraction()*Second

                  case _ =>
                    date.at(Clockface(Base24(hour), Base60(minute), Base60(second)))
                    . instant

              case _ =>
                date.at(Clockface(Base24(hour), Base60(minute), Base60(0))).instant

          case '.' | ',' =>
            date.at(Clockface(Base24(hour), Base60(0), Base60(0))).instant + fraction()*Hour

          case _ =>
            date.at(Clockface(Base24(hour), Base60(0), Base60(0))).instant

      case _ =>
        date.at(0.00.am).instant


    focus match
      case 'Z' | '\u0000' => instant

      case '+' | '-' =>
        val negate = focus == '-'
        val hour = next() yet number(2)

        if next() == ':' then next()
        val minute = number(2)
        instant + (if negate then hour*Hour + minute*Minute else -hour*Hour - minute*Minute)

      case _ =>
         abort(TimeError(_.Format(text, Iso8601, index)(Timezone)))
