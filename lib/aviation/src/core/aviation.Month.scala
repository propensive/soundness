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
┃    Soundness, version 0.33.0.                                                                    ┃
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
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*

object Month:
  val all: IArray[Month] = IArray(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)

  def apply(index: Int): Month raises TimeError =
    if index < 1 || index > 12 then abort(TimeError(_.Unknown(index.show, t"month")))
    else all(index - 1)

  def apply(name: Text): Month raises TimeError =
    try Month.valueOf(name.s) catch case _: Exception => abort(TimeError(_.Unknown(name, t"month")))

  def unapply(value: Text): Option[Month] =
    try Some(Month.valueOf(value.lower.capitalize.s))
    catch case err: IllegalArgumentException => None

  def unapply(value: Int): Option[Month] =
    if value < 1 || value > 12 then None else Some(fromOrdinal(value - 1))

  given monthOfYear: Int is Subtractable by Month into Monthstamp = new Subtractable:
    type Self = Int
    type Result = Monthstamp
    type Operand = Month

    def subtract(year: Int, month: Month) = new Monthstamp(Year(year), month)

  given showable: (months: Months) => Month is Showable = months.name(_)

  given subtractable: Month is Subtractable:
    type Operand = Int
    type Result = Anniversary

    def subtract(month: Month, day: Int): Anniversary = Anniversary(month, Day(day))

enum Month:
  case Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec

  def numerical: Int = ordinal + 1

  def offset(leapYear: Boolean): Int = (if leapYear && ordinal > 1 then 1 else 0) + this.match
    case Jan => 0
    case Feb => 31
    case Mar => 59
    case Apr => 90
    case May => 120
    case Jun => 151
    case Jul => 181
    case Aug => 212
    case Sep => 243
    case Oct => 273
    case Nov => 304
    case Dec => 334
