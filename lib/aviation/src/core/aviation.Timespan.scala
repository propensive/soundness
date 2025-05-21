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
┃    Soundness, version 0.31.0 for Scala 3.7.                                                      ┃
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
import proscenium.*
import symbolism.*

object Timespan:
  given genericDuration: Timespan is GenericDuration & SpecificDuration =
    new GenericDuration with SpecificDuration:
      type Self = Timespan
      def duration(milliseconds: Long): Timespan =
        val hours: Int = (milliseconds/3600000L).toInt
        val minutes: Int = ((milliseconds%3600000L)/60000L).toInt
        val seconds: Int = ((milliseconds%60000L)/1000L).toInt

        new Timespan(0, 0, 0, hours, minutes, seconds)

      def milliseconds(period: Timespan): Long =
        period.hours*3600000L + period.minutes*60000L + period.seconds*1000L

  def apply(denomination: StandardTime, n: Int): Timespan = denomination.absolve match
    case StandardTime.Year   => Timespan(n, 0, 0, 0, 0, 0)
    case StandardTime.Month  => Timespan(0, n, 0, 0, 0, 0)
    case StandardTime.Day    => Timespan(0, 0, n, 0, 0, 0)
    case StandardTime.Hour   => Timespan(0, 0, 0, n, 0, 0)
    case StandardTime.Minute => Timespan(0, 0, 0, 0, n, 0)
    case StandardTime.Second => Timespan(0, 0, 0, 0, 0, n)

  def fixed
       (denomination: StandardTime.Second.type | StandardTime.Minute.type | StandardTime.Hour.type,
        n: Int)
  :     Timespan =
    denomination match
      case StandardTime.Hour   => new Timespan(0, 0, 0, n, 0, 0)
      case StandardTime.Minute => new Timespan(0, 0, 0, 0, n, 0)
      case StandardTime.Second => new Timespan(0, 0, 0, 0, 0, n)

  given addable: Chronology[StandardTime] => Timespan is Addable:
    type Result = Timespan
    type Operand = Timespan
    def add(left: Timespan, right: Timespan): Timespan =
      Timespan
       (left.years + right.years,
        left.months + right.months,
        left.days + right.days,
        left.hours + right.hours,
        left.minutes + right.minutes,
        left.seconds + right.seconds)

  given subtractable: Chronology[StandardTime] => Timespan is Subtractable:
    type Result = Timespan
    type Operand = Timespan

    def subtract(left: Timespan, right: Timespan): Timespan =
      Timespan
       (left.years - right.years,
        left.months - right.months,
        left.days - right.days,
        left.hours - right.hours,
        left.minutes - right.minutes,
        left.seconds - right.seconds)

  given multiplicable: Chronology[StandardTime] => Timespan is Multiplicable:
    type Operand = Int
    type Result = Timespan

    def multiply(left: Timespan, right: Int): Timespan =
      Timespan
       (left.years*right,
        left.months*right,
        left.days*right,
        left.hours*right,
        left.minutes*right,
        left.seconds*right)

case class Timespan(years: Int, months: Int, days: Int, hours: Int, minutes: Int, seconds: Int):
  def simplify(using chronology: Chronology[StandardTime]): Timespan = chronology.simplify(this)
