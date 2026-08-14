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
import hieroglyph.*, textMetrics.uniformMetric
import spectacular.*

object Clockface:
  given showable: (Format, Numerics, Separation, Specificity)
  =>  Clockface is Showable =

    clockface =>
      val hour =
        if summon[Format].halfDay then
          val raw = clockface.hour%12
          if raw == 0 then 12 else raw
        else
          clockface.hour

      val hour2 = summon[Numerics] match
        case Numerics.VariableWidth => hour.show
        case Numerics.FixedWidth    => hour.show.pad(2, Bidi.Rtl, '0')

      val minute = (clockface.minute: Int).show.pad(2, Bidi.Rtl, '0')

      val seconds0 = (clockface.second: Int).show.pad(2, Bidi.Rtl, '0')

      val seconds =
        if !summon[Format].seconds then t""
        else t"${summon[Separation].secondSeparator}$seconds0"

      val meridiem = (clockface.hour/12) match
        case 0 => Meridiem.Am
        case _ => Meridiem.Pm

      val postfix = summon[Format].postfix(meridiem)

      t"$hour2${summon[Separation].separator}$minute$seconds$postfix"

  // TimeFormat → Format
  trait Format:
    def halfDay: Boolean
    def seconds: Boolean
    def postfix(meridiem: Meridiem): Text

  // TimeNumerics → Numerics
  enum Numerics:
    case FixedWidth, VariableWidth

  // TimeSpecificity → Specificity
  enum Specificity:
    case Minutes, Seconds

  // TimeSeparation → Separation
  trait Separation:
    def separator: Text = separatorChar()
    def separatorChar(): Text
    def secondSeparator: Text = separator

case class Clockface(hour: Base24, minute: Base60, second: Base60 = 0, nanos: Int = 0):
  infix def on(date: Date)(using Calendar): Timestamp = Timestamp(date, this)
