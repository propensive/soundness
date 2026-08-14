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

import scala.io.*

import anticipation.*
import proscenium.compat.*
import rudiments.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import kaleidoscope.*
import spectacular.*
import symbolism.*
import vacuous.*
import fulminate.*

object Tzdb:
  case class Time(hours: Int, minutes: Int, seconds: Int, suffix: Optional[Char])
  case class Duration(hours: Int, minutes: Int, seconds: Int)

  enum Entry:
    case Rule
      ( name:    Text,
        from:    Int,
        end:     Int,
        change:  MonthDate,
        time:    Time,
        save:    Duration,
        letters: Option[Text] )

    case Leap(year: Int, month: Month, day: Int, time: Time, addition: Boolean)
    case Zone(area: Text, location: Option[Text], info: Sequence[ZoneInfo])
    case Link(from: Text, to: Text)

  case class ZoneInfo
    ( stdoff: into[Duration], rules: Text, format: Text => Text, until: Option[Text] )

  enum MonthDate:
    case Last(month: Month, day: Weekday)
    case Exact(month: Month, day: Int)
    case After(month: Month, day: Weekday, date: Int)
    case Before(month: Month, day: Weekday, date: Int)

  def parseFile(name: Text): List[Tzdb.Entry] logs Tzdb.Event raises Tzdb.Error =
    Log.fine(Tzdb.Event.ParseTzdb(name))

    val lines: Chain[Text] =
      val stream = safely(getClass.getResourceAsStream(s"/aviation/tzdb/$name").nn)

      val stream2 = stream.or:
        abort(Tzdb.Error(Tzdb.Error.Reason.NoTzdbFile(name), 0))

      Source.fromInputStream(stream2).getLines().map(Text(_)).map(_.cut(t"\t").stdlib.head.lower)
      . to(Chain)

    parse(name, lines)

  def parse(name: Text, lines: Chain[Text]): List[Tzdb.Entry] logs Tzdb.Event raises Tzdb.Error =
    def parseDuration(lineNo: Int, string: Text) = string.cut(t":") match
      case As[Base24](h) :: Nil                                   => Duration(h, 0, 0)
      case As[Base24](h) :: As[Base60](m) :: Nil                  => Duration(h, m, 0)
      case As[Base24](h) :: As[Base60](m) :: As[Base60](s) :: Nil => Duration(h, m, s)

      case other =>
        abort(Tzdb.Error(Tzdb.Error.Reason.CouldNotParseTime(other.show), lineNo))

    def parseTime(lineNo: Int, string: Text) = string.cut(t":") match
      case As[Base24](h) :: r"${As[Base60](m)}([0-9]*)s" :: Nil   => Time(h, m, 0, 's')
      case As[Base24](h) :: r"${As[Base60](m)}([0-9]*)u" :: Nil   => Time(h, m, 0, 'u')
      case As[Base24](h) :: As[Base60](m) :: Nil                  => Time(h, m, 0, Unset)
      case As[Base24](h) :: As[Base60](m) :: As[Base60](s) :: Nil => Time(h, m, s, Unset)

      case other =>
        abort(Tzdb.Error(Tzdb.Error.Reason.CouldNotParseTime(other.show), lineNo))

    def parseDay(lineNo: Int, month: Month, string: Text): MonthDate =
      try throwErrors:
        if string.starts(t"last") then MonthDate.Last(month, Weekday.valueOf(string.skip(4).s))
        else if string.skip(3).keep(2) == t">="
        then MonthDate.After(month, Weekday.valueOf(string.keep(3).s), string.skip(5).as[Int])
        else if string.skip(3).keep(2) == t"<="
        then MonthDate.Before(month, Weekday.valueOf(string.keep(3).s), string.skip(5).as[Int])
        else MonthDate.Exact(month, string.as[Int])
      catch case error: Number.Error =>
        abort(Tzdb.Error(Tzdb.Error.Reason.UnparsableDate, lineNo))

    def parseLeap(lineNo: Int, arguments: List[Text]): Tzdb.Entry.Leap = arguments match
      case As[Int](year) :: month :: As[Int](day) :: time :: add :: s :: Nil =>
        Tzdb.Entry.Leap(year, parseMonth(month), day, parseTime(lineNo, time), add == t"+")

      case other =>
        abort(Tzdb.Error(Tzdb.Error.Reason.UnexpectedRule, lineNo))

    def parseMonth(string: Text) = Month.valueOf(string.s)

    def parseZone(lineNo: Int, arguments: List[Text]): Tzdb.Entry.Zone = arguments match
      case name :: rest =>
        name.cut(t"/", 2) match
          case area :: location :: Nil =>
            Tzdb.Entry.Zone(area, Some(location), Sequence(parseZoneInfo(lineNo, rest)))

          case simple :: Nil =>
            Tzdb.Entry.Zone(simple, None, Sequence(parseZoneInfo(lineNo, rest)))

          case _ =>
            abort(Tzdb.Error(Tzdb.Error.Reason.BadName(name), lineNo))

      case _ =>
        abort(Tzdb.Error(Tzdb.Error.Reason.UnexpectedRule, lineNo))

    def parseZoneInfo(lineNo: Int, arguments: List[Text]): Tzdb.ZoneInfo = arguments match
      case stdoff :: rules :: format :: until =>
        val s = parseDuration(lineNo, stdoff)

        def f(string: Text) = format.cut(t"%s", 2).absolve match
          case value :: Nil           => value
          case before :: after :: Nil => before+string+after

        ZoneInfo(s, rules, f, if until.nil then None else Some(until.join(t" ")))

      case other =>
        abort(Tzdb.Error(Tzdb.Error.Reason.BadZoneInfo(other), lineNo))

    def parseLetters(string: Text): Option[Text] = if string == t"-" then None else Some(string)

    def parseRule(lineNo: Int, arguments: List[Text]): Tzdb.Entry.Rule = arguments match
      case name :: from :: to :: _ :: month :: day :: time :: save :: letters :: _ =>
        try unsafely:
          val end = to match
            case t"max"  => Int.MaxValue
            case t"only" => from.as[Int]
            case other   => to.as[Int]

          val d = parseDay(lineNo, parseMonth(month), day)
          val t = parseTime(lineNo, time)
          val s = parseDuration(lineNo, save)
          Tzdb.Entry.Rule(name, from.as[Int], end, d, t, s, parseLetters(letters))

        catch case error: Number.Error =>
          abort(Tzdb.Error(Tzdb.Error.Reason.UnexpectedRule, lineNo))

      case _ =>
        abort(Tzdb.Error(Tzdb.Error.Reason.UnexpectedRule, lineNo))

    def parseLink(lineNo: Int, arguments: List[Text]): Tzdb.Entry.Link = arguments match
      case from :: to :: Nil => Tzdb.Entry.Link(from, to)
      case _                 => abort(Tzdb.Error(Tzdb.Error.Reason.UnexpectedLink, lineNo))

    def addToZone(lineNo: Int, arguments: List[Text], zone: Tzdb.Entry.Zone): Tzdb.Entry.Zone =
      zone.copy(info = Sequence.of(zone.info.stdlib :+ parseZoneInfo(lineNo, arguments)))

    @tailrec
    def recur
      ( lineNo:  Int,
        lines:   Chain[Text],
        entries: List[Tzdb.Entry]        = Nil,
        zone:    Option[Tzdb.Entry.Zone] = None )
    :   List[Tzdb.Entry] =

      if lines.nil then List.of(entries.stdlib ++ zone.toList) else
        val line: Text = lines.head.upto(_ == '#')

        line.cut(unsafely(r"\s+")) match
          case t"Rule" :: tail =>
            recur(lineNo + 1, lines.tail, parseRule(lineNo, tail) :: List.of(zone.toList ++ entries.stdlib))

          case t"Link" :: tail =>
            recur(lineNo + 1, lines.tail, parseLink(lineNo, tail) :: List.of(zone.toList ++ entries.stdlib))

          case t"Zone" :: tail =>
            recur(lineNo + 1, lines.tail, List.of(entries.stdlib ++ zone.toList), Some(parseZone(lineNo, tail)))

          case t"Leap" :: tail =>
            recur(lineNo + 1, lines.tail, parseLeap(lineNo, tail) :: List.of(zone.toList ++ entries.stdlib))

          case t"" :: Nil =>
            recur(lineNo + 1, lines.tail, entries, zone)

          case t"" :: tail =>
            recur(lineNo + 1, lines.tail, entries, Some(addToZone(lineNo, tail, zone.getOrElse:
              abort(Tzdb.Error(Tzdb.Error.Reason.UnexpectedZoneInfo, lineNo)))))

          case other =>
            recur(lineNo + 1, lines.tail, entries, zone)

    recur(1, lines)

  // TzdbError → Tzdb.Error
  object Error:
    given communicable: Reason is Communicable =
      case Reason.CouldNotParseTime(time) => m"could not parse time $time"
      case Reason.UnexpectedRule          => m"unexpected rule"
      case Reason.UnexpectedLink          => m"unexpected link"
      case Reason.UnexpectedZoneInfo      => m"unexpected zone info"
      case Reason.BadZoneInfo(line)       => m"bad zone information: ${line.join(t"[", t"   ", t"]")}"
      case Reason.BadName(name)           => m"the name $name is not valid"
      case Reason.UnparsableDate          => m"the date could not be parsed"
      case Reason.NoTzdbFile(name)        => m"the zonefile $name could not be found on the classpath"

    enum Reason(val number: Int) extends Clarification:
      case CouldNotParseTime(time: Text) extends Reason(1)
      case UnexpectedRule extends Reason(2)
      case UnexpectedLink extends Reason(3)
      case UnexpectedZoneInfo extends Reason(4)
      case UnparsableDate extends Reason(5)
      case BadZoneInfo(line: List[Text]) extends Reason(6)
      case BadName(name: Text) extends Reason(7)
      case NoTzdbFile(name: Text) extends Reason(8)

  case class Error(reason: Tzdb.Error.Reason, line: Int)(using Diagnostics)
  extends fulminate.Error(385, reason.number)
    ( m"the timezone could not be parsed at line $line: $reason" )

  // TimeEvent → Tzdb.Event
  object Event:
    given communicable: Event is Communicable =
      case ParseTzdb(name) => m"parsing the timezone database file $name"

  enum Event:
    case ParseTzdb(name: Text) extends Event, Log.Time

