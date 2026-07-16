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
package facsimile

import anticipation.*
import aviation.*
import contingency.*
import quantitative.*
import vacuous.*

object PdfInfo:
  // A PDF date (ISO 32000-2 §7.9.4): local time with an *optional* UTC offset — absence
  // means the relationship to UTC is unknown, so a zoneless `Timestamp` carries the moment
  // and the offset rides alongside only when the file stated one.
  case class Timing(timestamp: Timestamp, offset: Optional[Duration])

  // `D:YYYYMMDDHHmmSS±HH'mm'`, everything after the year optional; a malformed date is
  // `Unset`, never an error, since real files abound with slightly-wrong dates.
  private[facsimile] def parseDate(value: Text): Optional[Timing] =
    val content = if value.s.startsWith("D:") then value.s.substring(2).nn else value.s

    def digits(start: Int, length: Int, minimum: Int, maximum: Int): Optional[Int] =
      if start + length > content.length then Unset else
        var i = start
        var number = 0
        var bad = false

        while i < start + length do
          val char = content.charAt(i)
          if char < '0' || char > '9' then bad = true else number = number*10 + (char - '0')
          i += 1

        if bad || number < minimum || number > maximum then Unset else number

    digits(0, 4, 0, 9999).let: year =>
      val month = digits(4, 2, 1, 12).or(1)
      val day = digits(6, 2, 1, 31).or(1)
      val hour = digits(8, 2, 0, 23).or(0)
      val minute = digits(10, 2, 0, 59).or(0)
      val second = digits(12, 2, 0, 59).or(0)

      val offset: Optional[Duration] =
        if content.length > 14 then content.charAt(14) match
          case 'Z' =>
            Quantity[Seconds[1]](0.0)

          case sign @ ('+' | '-') =>
            digits(15, 2, 0, 23).let: hours =>
              val minutes = digits(18, 2, 0, 59).or(0)
              val seconds = (hours*3600 + minutes*60)*(if sign == '-' then -1 else 1)
              Quantity[Seconds[1]](seconds.toDouble)

          case _ =>
            Unset
        else Unset

      import calendars.gregorianCalendar

      safely(Timestamp(Date(Year(year), Month(month), Day(day)),
          Clockface(Base24(hour), Base60(minute), Base60(second)))).let: timestamp =>
        Timing(timestamp, offset)

// The document-information dictionary, fully materialized: a pure value that outlives the
// `open` scope.
case class PdfInfo
  ( title:    Optional[Text],
    author:   Optional[Text],
    subject:  Optional[Text],
    keywords: Optional[Text],
    creator:  Optional[Text],
    producer: Optional[Text],
    created:  Optional[PdfInfo.Timing],
    modified: Optional[PdfInfo.Timing] )
