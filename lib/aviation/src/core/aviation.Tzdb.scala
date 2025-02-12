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
import gossamer.*
import kaleidoscope.*
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*

import scala.io.*

object Tzdb:
  case class Time(hours: Int, minutes: Int, seconds: Int, suffix: Optional[Char])
  case class Duration(hours: Int, minutes: Int, seconds: Int)

  enum Entry:
    case Rule
       (name:    Text,
        from:    Int,
        end:    Int,
        change:  MonthDate,
        time:    Time,
        save:    Duration,
        letters: Option[Text])

    case Leap(year: Int, month: MonthName, day: Int, time: Time, addition: Boolean)
    case Zone(area: Text, location: Option[Text], info: Trie[ZoneInfo])
    case Link(from: Text, to: Text)

  case class ZoneInfo(stdoff: Duration, rules: Text, format: Text => Text, until: Option[Text])

  enum MonthDate:
    case Last(month: MonthName, day: Weekday)
    case Exact(month: MonthName, day: Int)
    case After(month: MonthName, day: Weekday, date: Int)
    case Before(month: MonthName, day: Weekday, date: Int)

  def parseFile(name: Text): List[Tzdb.Entry] logs TimeEvent raises TzdbError =
    val lines: Stream[Text] =
      val stream = safely(getClass.getResourceAsStream(s"/aviation/tzdb/$name").nn)
      val stream2 = stream.or:
        abort(TzdbError(TzdbError.Reason.ZoneFileMissing(name), 0))

      Source.fromInputStream(stream2).getLines.map(Text(_)).map(_.cut(t"\t").head.lower)
      . to(Stream)

    parse(name, lines)

  def parse(name: Text, lines: Stream[Text]): List[Tzdb.Entry] logs TimeEvent raises TzdbError =
    def parseDuration(lineNo: Int, str: Text) = str.cut(t":").to(List) match
      case As[Base24](h) :: Nil                                   => Duration(h, 0, 0)
      case As[Base24](h) :: As[Base60](m) :: Nil                  => Duration(h, m, 0)
      case As[Base24](h) :: As[Base60](m) :: As[Base60](s) :: Nil => Duration(h, m, s)

      case other =>
        abort(TzdbError(TzdbError.Reason.CouldNotParseTime(other.show), lineNo))

    def parseTime(lineNo: Int, str: Text) = str.cut(t":").to(List) match
      case As[Base24](h) :: r"${As[Base60](m)}([0-9]*)s" :: Nil   => Time(h, m, 0, 's')
      case As[Base24](h) :: r"${As[Base60](m)}([0-9]*)u" :: Nil   => Time(h, m, 0, 'u')
      case As[Base24](h) :: As[Base60](m) :: Nil                  => Time(h, m, 0, Unset)
      case As[Base24](h) :: As[Base60](m) :: As[Base60](s) :: Nil => Time(h, m, s, Unset)

      case other =>
        abort(TzdbError(TzdbError.Reason.CouldNotParseTime(other.show), lineNo))

    def parseDay(lineNo: Int, month: MonthName, str: Text): MonthDate =
      try throwErrors:
        if str.starts(t"last") then MonthDate.Last(month, Weekday.valueOf(str.skip(4).s))
        else if str.skip(3).keep(2) == t">="
        then MonthDate.After(month, Weekday.valueOf(str.keep(3).s), str.skip(5).decode[Int])
        else if str.skip(3).keep(2) == t"<="
        then MonthDate.Before(month, Weekday.valueOf(str.keep(3).s), str.skip(5).decode[Int])
        else MonthDate.Exact(month, str.decode[Int])
      catch case err: NumberError =>
        abort(TzdbError(TzdbError.Reason.UnparsableDate, lineNo))

    def parseLeap(lineNo: Int, args: List[Text]): Tzdb.Entry.Leap = args match
      case As[Int](year) :: month :: As[Int](day) :: time :: add :: s :: Nil =>
        Tzdb.Entry.Leap(year, parseMonth(month), day, parseTime(lineNo, time), add == t"+")

      case other =>
        abort(TzdbError(TzdbError.Reason.UnexpectedRule, lineNo))

    def parseMonth(str: Text) = MonthName.valueOf(str.s)

    def parseZone(lineNo: Int, args: List[Text]): Tzdb.Entry.Zone = args match
      case name :: rest =>
        name.cut(t"/", 2).to(List) match
          case area :: location :: Nil =>
            Tzdb.Entry.Zone(area, Some(location), Trie(parseZoneInfo(lineNo, rest)))

          case simple :: Nil =>
            Tzdb.Entry.Zone(simple, None, Trie(parseZoneInfo(lineNo, rest)))

          case _ =>
            abort(TzdbError(TzdbError.Reason.BadName(name), lineNo))
      case _ =>
        abort(TzdbError(TzdbError.Reason.UnexpectedRule, lineNo))

    def parseZoneInfo(lineNo: Int, args: List[Text]): Tzdb.ZoneInfo = args match
      case stdoff :: rules :: format :: until =>
        val s = parseDuration(lineNo, stdoff)

        def f(str: Text) = format.cut(t"%s", 2).to(List).absolve match
          case value :: Nil           => value
          case before :: after :: Nil => before+str+after

        ZoneInfo(s, rules, f, if until.isEmpty then None else Some(until.join(t" ")))

      case other =>
        abort(TzdbError(TzdbError.Reason.BadZoneInfo(other), lineNo))

    def parseLetters(str: Text): Option[Text] = if str == t"-" then None else Some(str)

    def parseRule(lineNo: Int, args: List[Text]): Tzdb.Entry.Rule = args match
      case name :: from :: to :: _ :: month :: day :: time :: save :: letters :: _ =>
        try unsafely:
          val end = to match
            case t"max"  => Int.MaxValue
            case t"only" => from.decode[Int]
            case other   => to.decode[Int]

          val d = parseDay(lineNo, parseMonth(month), day)
          val t = parseTime(lineNo, time)
          val s = parseDuration(lineNo, save)
          Tzdb.Entry.Rule(name, from.decode[Int], end, d, t, s, parseLetters(letters))

        catch case err: NumberError =>
          abort(TzdbError(TzdbError.Reason.UnexpectedRule, lineNo))

      case _ =>
        abort(TzdbError(TzdbError.Reason.UnexpectedRule, lineNo))

    def parseLink(lineNo: Int, args: List[Text]): Tzdb.Entry.Link = args match
      case from :: to :: Nil => Tzdb.Entry.Link(from, to)
      case _                 => abort(TzdbError(TzdbError.Reason.UnexpectedLink, lineNo))

    def addToZone(lineNo: Int, args: List[Text], zone: Tzdb.Entry.Zone): Tzdb.Entry.Zone =
      zone.copy(info = zone.info :+ parseZoneInfo(lineNo, args))

    @tailrec
    def recur
       (lineNo:  Int,
        lines:   Stream[Text],
        entries: List[Tzdb.Entry]        = Nil,
        zone:    Option[Tzdb.Entry.Zone] = None)
    :     List[Tzdb.Entry] =
      if lines.isEmpty then entries ++ zone
      else
        val line: Text = lines.head.upto(_ == '#')
        line.cut(unsafely(r"\s+")).to(List) match
          case t"Rule" :: tail =>
            recur(lineNo + 1, lines.tail, parseRule(lineNo, tail) :: (zone.to(List) ++ entries))

          case t"Link" :: tail =>
            recur(lineNo + 1, lines.tail, parseLink(lineNo, tail) :: (zone.to(List) ++ entries))

          case t"Zone" :: tail =>
            recur(lineNo + 1, lines.tail, entries ++ zone.to(List), Some(parseZone(lineNo, tail)))

          case t"Leap" :: tail =>
            recur(lineNo + 1, lines.tail, parseLeap(lineNo, tail) :: (zone.to(List) ++ entries))

          case t"" :: Nil =>
            recur(lineNo + 1, lines.tail, entries, zone)

          case t"" :: tail =>
            recur(lineNo + 1, lines.tail, entries, Some(addToZone(lineNo, tail, zone.getOrElse:
              abort(TzdbError(TzdbError.Reason.UnexpectedZoneInfo, lineNo)))))

          case other =>
            recur(lineNo + 1, lines.tail, entries, zone)

    recur(1, lines)
