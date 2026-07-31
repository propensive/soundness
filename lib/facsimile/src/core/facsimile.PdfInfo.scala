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
package facsimile

import proscenium.compat.*

import anticipation.*
import aviation.*
import contingency.*
import gossamer.*
import quantitative.*
import rudiments.*
import vacuous.*

object PdfInfo:
  // Serialises document information back to an `/Info` dictionary: text fields as PDF text
  // strings, dates in the `D:` form. Absent fields are omitted.
  private[facsimile] def dictionary(info: PdfInfo): Map[Text, Cos] =
    var entries = Map[Text, Cos]()

    def string(key: Text, value: Optional[Text]): Unit =
      value.let { text => entries = entries.updated(key, Cos.Chars(Cos.encodeText(text))) }

    def date(key: Text, value: Optional[Timing]): Unit =
      value.let { timing => entries = entries.updated(key, Cos.Chars(Cos.encodeText(formatDate(timing)))) }

    string(t"Title", info.title)
    string(t"Author", info.author)
    string(t"Subject", info.subject)
    string(t"Keywords", info.keywords)
    string(t"Creator", info.creator)
    string(t"Producer", info.producer)
    date(t"CreationDate", info.created)
    date(t"ModDate", info.modified)
    entries

  private def formatDate(timing: Timing): Text =
    import calendars.gregorianCalendar
    val ts = timing.timestamp

    def pad(n: Int, width: Int): Text =
      val digits = n.toString
      ("0".repeat(width - digits.length).nn + digits).tt

    val year: Int = ts.year.apply()
    val month: Int = ts.month.ordinal + 1
    val day: Int = ts.day.apply()

    val stamp =
      t"D:${pad(year, 4)}${pad(month, 2)}${pad(day, 2)}${pad(ts.hour, 2)}${pad(ts.minute, 2)}${pad(ts.second, 2)}"

    val zone = timing.offset.lay(t""): duration =>
      val seconds = duration.value.toInt

      if seconds == 0 then t"Z" else
        val minutes = (if seconds < 0 then -seconds else seconds)/60
        t"${if seconds < 0 then t"-" else t"+"}${pad(minutes/60, 2)}'${pad(minutes%60, 2)}'"

    t"$stamp$zone"
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
