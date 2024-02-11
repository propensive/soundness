/*
    Aviation, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package aviation

import gossamer.*
import kaleidoscope.*
import anticipation.*
import spectacular.*
import eucalyptus.*
import rudiments.*
import vacuous.*
import contingency.*
import fulminate.*

import scala.io.*

//import language.experimental.captureChecking

object TzdbError:
  given Communicable[Reason] =
    case Reason.CouldNotParseTime(time) => msg"could not parse time $time"
    case Reason.UnexpectedRule          => msg"unexpected rule"
    case Reason.UnexpectedLink          => msg"unexpected link"
    case Reason.UnexpectedZoneInfo      => msg"unexpected zone info"
    case Reason.BadZoneInfo(line)       => msg"bad zone information: ${line.join(t"[", t"   ", t"]")}"
    case Reason.BadName(name)           => msg"the name $name is not valid"
    case Reason.UnparsableDate          => msg"the date could not be parsed"
    case Reason.ZoneFileMissing(name)   => msg"the zone file $name could not be found on the classpath"

  enum Reason:
    case CouldNotParseTime(time: Text)
    case UnexpectedRule
    case UnexpectedLink
    case UnexpectedZoneInfo
    case UnparsableDate
    case BadZoneInfo(line: List[Text])
    case BadName(name: Text)
    case ZoneFileMissing(name: Text)

case class TzdbError(reason: TzdbError.Reason, line: Int)
extends Error(msg"the timezone could not be parsed at line $line: $reason")

object Tzdb:
  case class Time(hours: Int, minutes: Int, seconds: Int, suffix: Optional[Char])
  case class Duration(hours: Int, minutes: Int, seconds: Int)
  
  enum Entry:
    case Rule(name: Text, from: Int, end: Int, change: MonthDate, time: Time,
                  save: Duration, letters: Option[Text])
    case Leap(year: Int, month: MonthName, day: Int, time: Time, addition: Boolean)
    case Zone(area: Text, location: Option[Text], info: Vector[ZoneInfo])
    case Link(from: Text, to: Text)

  case class ZoneInfo(stdoff: Duration, rules: Text, format: Text => Text,
                          until: Option[Text])

  enum MonthDate:
    case Last(month: MonthName, day: Weekday)
    case Exact(month: MonthName, day: Int)
    case After(month: MonthName, day: Weekday, date: Int)
    case Before(month: MonthName, day: Weekday, date: Int)

  def parseFile(name: Text)(using Log[Text], Raises[TzdbError]): List[Tzdb.Entry] =
    val lines: LazyList[Text] =
      val stream = safely(getClass.getResourceAsStream(s"/aviation/tzdb/$name").nn).or:
        abort(TzdbError(TzdbError.Reason.ZoneFileMissing(name), 0))

      Source.fromInputStream(stream).getLines.map(Text(_)).map(_.cut(t"\t").head.lower).to(LazyList)

    parse(name, lines)

  def parse(name: Text, lines: LazyList[Text])(using Log[Text], Raises[TzdbError]): List[Tzdb.Entry] =
    
    def parseDuration(lineNo: Int, str: Text) = str.cut(t":").to(List) match
      case As[Int](h) :: Nil                             => Duration(h, 0, 0)
      case As[Int](h) :: As[Int](m) :: Nil               => Duration(h, m, 0)
      case As[Int](h) :: As[Int](m) :: As[Int](s) :: Nil => Duration(h, m, s)
      
      case other =>
        abort(TzdbError(TzdbError.Reason.CouldNotParseTime(other.show), lineNo))

    def parseTime(lineNo: Int, str: Text) = str.cut(t":").to(List) match
      case As[Int](h) :: r"${As[Int](m)}([0-9]*)s" :: Nil => Time(h, m, 0, 's')
      case As[Int](h) :: r"${As[Int](m)}([0-9]*)u" :: Nil => Time(h, m, 0, 'u')
      case As[Int](h) :: As[Int](m) :: Nil                => Time(h, m, 0, Unset)
      case As[Int](h) :: As[Int](m) :: As[Int](s) :: Nil  => Time(h, m, s, Unset)
      
      case other =>
        abort(TzdbError(TzdbError.Reason.CouldNotParseTime(other.show), lineNo))

    def parseDay(lineNo: Int, month: MonthName, str: Text): MonthDate =
      try throwErrors:
        if str.starts(t"last") then MonthDate.Last(month, Weekday.valueOf(str.drop(4).s))
        else if str.drop(3).take(2) == t">="
        then MonthDate.After(month, Weekday.valueOf(str.take(3).s), str.drop(5).decodeAs[Int])
        else if str.drop(3).take(2) == t"<="
        then MonthDate.Before(month, Weekday.valueOf(str.take(3).s), str.drop(5).decodeAs[Int])
        else MonthDate.Exact(month, str.decodeAs[Int])
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
            Tzdb.Entry.Zone(area, Some(location), Vector(parseZoneInfo(lineNo, rest)))
          
          case simple :: Nil =>
            Tzdb.Entry.Zone(simple, None, Vector(parseZoneInfo(lineNo, rest)))
          
          case _ =>
            abort(TzdbError(TzdbError.Reason.BadName(name), lineNo))
      case _ =>
        abort(TzdbError(TzdbError.Reason.UnexpectedRule, lineNo))

    def parseZoneInfo(lineNo: Int, args: List[Text]): Tzdb.ZoneInfo = args match
      case stdoff :: rules :: format :: until =>
        val s = parseDuration(lineNo, stdoff)
        
        def f(str: Text) = (format.cut(t"%s", 2).to(List): @unchecked) match
          case value :: Nil           => value
          case before :: after :: Nil => before+str+after

        ZoneInfo(s, rules, f, if until.isEmpty then None else Some(until.join(t" ")))
      
      case other =>
        abort(TzdbError(TzdbError.Reason.BadZoneInfo(other), lineNo))

    def parseLetters(str: Text): Option[Text] = if str == t"-" then None else Some(str)

    def parseRule(lineNo: Int, args: List[Text]): Tzdb.Entry.Rule = args match
      case name :: from :: to :: _ :: month :: day :: time :: save :: letters :: _ =>
        try throwErrors:
          val end = to match
            case t"max"  => Int.MaxValue
            case t"only" => from.decodeAs[Int]
            case other   => to.decodeAs[Int]
          
          val d = parseDay(lineNo, parseMonth(month), day)
          val t = parseTime(lineNo, time)
          val s = parseDuration(lineNo, save)
          Tzdb.Entry.Rule(name, from.decodeAs[Int], end, d, t, s, parseLetters(letters))
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
        (lineNo: Int, lines: LazyList[Text], entries: List[Tzdb.Entry] = Nil,
            zone: Option[Tzdb.Entry.Zone] = None)
        : List[Tzdb.Entry] =
      if lines.isEmpty then
        //Log.fine(t"Finished parsing $lineNo lines of $name, and got ${entries.size} entries")
        entries ++ zone
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

given realm: Realm = realm"aviation"
