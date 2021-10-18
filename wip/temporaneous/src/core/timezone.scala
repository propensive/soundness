package temporaneous

import gossamer.*
import kaleidoscope.*
import eucalyptus.*
import escapade.*
import rudiments.*

import scala.annotation.*
import unsafeExceptions.canThrowAny

case class TzParseError(msg: String, line: Int)
extends Exception(s"temporaneous: the timezone could not be parsed at line $line: $msg")

enum Weekday:
  case Mon, Tue, Wed, Thu, Fri, Sat, Sun

enum Month:
  case Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec

case class Time(hours: Int, minutes: Int, seconds: Int, suffix: Maybe[Char])
case class Duration(hours: Int, minutes: Int, seconds: Int)

object Tzdb:
  enum Entry:
    case Rule(name: String, from: Int, end: Int, change: MonthDate, time: Time,
                  save: Duration, letters: Option[String])
    case Leap(year: Int, month: Month, day: Int, time: Time, addition: Boolean)
    case Zone(area: String, location: Option[String], info: Vector[ZoneInfo])
    case Link(from: String, to: String)

  case class ZoneInfo(stdoff: Duration, rules: String, format: String => String,
                          until: Option[String])

  enum MonthDate:
    case Last(month: Month, day: Weekday)
    case Exact(month: Month, day: Int)
    case After(month: Month, day: Weekday, date: Int)
    case Before(month: Month, day: Weekday, date: Int)

  def parseFile(name: String)(using Log): List[Tzdb.Entry] =
    val lines: LazyList[String] =
      val stream = Option(getClass.getResourceAsStream(s"/temporaneous/$name")).getOrElse {
        throw Exception(s"could not find the file 'temporaneous/$name' on the classpath")
      }.nn

      scala.io.Source.fromInputStream(stream).getLines.map(_.cut("\t").head.lower).to(LazyList)

    parse(name, lines)

  def parse(name: String, lines: LazyList[String])(using Log): List[Tzdb.Entry] =
    
    def parseDuration(lineNo: Int, str: String) = str.cut(":").to(List) match
      case h :: Nil           => Duration(h.toInt, 0, 0)
      case h :: m :: Nil      => Duration(h.toInt, m.toInt, 0)
      case h :: m :: s :: Nil => Duration(h.toInt, m.toInt, s.toInt)
      case other              => throw TzParseError("Could not parse time "+other, lineNo)

    def parseTime(lineNo: Int, str: String) = str.cut(":").to(List) match
      case h :: m :: Nil if m.endsWith("s") => Time(h.toInt, m.dropRight(1).toInt, 0, 's')
      case h :: m :: Nil if m.endsWith("u") => Time(h.toInt, m.dropRight(1).toInt, 0, 'u')
      case h :: m :: Nil                    => Time(h.toInt, m.toInt, 0, Unset)
      case h :: m :: s :: Nil               => Time(h.toInt, m.toInt, s.toInt, Unset)
      case other         => throw TzParseError("Could not parse time "+other, lineNo)

    def parseDay(month: Month, str: String): MonthDate =
      if str.startsWith("last") then MonthDate.Last(month, Weekday.valueOf(str.drop(4)))
      else if str.drop(3).take(2) == ">="
      then MonthDate.After(month, Weekday.valueOf(str.take(3)), str.drop(5).toInt)
      else if str.drop(3).take(2) == "<="
      then MonthDate.Before(month, Weekday.valueOf(str.take(3)), str.drop(5).toInt)
      else MonthDate.Exact(month, str.toInt)

    def parseLeap(lineNo: Int, args: List[String]): Tzdb.Entry.Leap = args match
      case year :: month :: day :: time :: add :: s :: Nil =>
        Tzdb.Entry.Leap(year.toInt, parseMonth(month), day.toInt, parseTime(lineNo, time), add == "+")

    def parseMonth(str: String) = Month.valueOf(str)

    def parseZone(lineNo: Int, args: List[String]): Tzdb.Entry.Zone = args match
      case name :: rest =>
        name.cut("/", 2).to(List) match
          case area :: location :: Nil =>
            Tzdb.Entry.Zone(area, Some(location), Vector(parseZoneInfo(lineNo, rest)))
          
          case simple :: Nil =>
            Tzdb.Entry.Zone(simple, None, Vector(parseZoneInfo(lineNo, rest)))
          
          case _ =>
            throw TzParseError("Bad name: "+name, lineNo)
      case _                 =>
        throw TzParseError("Unexpected Rule", lineNo)

    def parseZoneInfo(lineNo: Int, args: List[String]): Tzdb.ZoneInfo = args match
      case stdoff :: rules :: format :: until =>
        val s = parseDuration(lineNo, stdoff)
        
        def f(str: String) = format.cut("%s", 2).to(List) match
          case value :: Nil           => value
          case before :: after :: Nil => before+str+after

        ZoneInfo(s, rules, f, if until.isEmpty then None else Some(until.join(" ")))
      
      case other =>
        throw TzParseError("Bad zone info: "+other, lineNo)

    def parseLetters(str: String): Option[String] = if str == "-" then None else Some(str)

    def parseRule(lineNo: Int, args: List[String]): Tzdb.Entry.Rule = args match
      case name :: from :: to :: _ :: month :: day :: time :: save :: letters :: _ =>
        val end = to match
          case "max"  => Int.MaxValue
          case "only" => from.toInt
          case other  => to.toInt
        
        val d = parseDay(parseMonth(month), day)
        //val d = try parseDay(day) catch Exception => throw TzParseError("Couldn't parse day '"+day+"'", lineNo)
        val t = parseTime(lineNo, time)
        val s = parseDuration(lineNo, save)
        Tzdb.Entry.Rule(name, from.toInt, end, d, t, s, parseLetters(letters))
      case _ =>
        throw TzParseError("Unexpected Rule", lineNo)

    def parseLink(lineNo: Int, args: List[String]): Tzdb.Entry.Link = args match
      case from :: to :: Nil => Tzdb.Entry.Link(from, to)
      case _                 => throw TzParseError("Unexpected Link", lineNo)

    def addToZone(lineNo: Int, args: List[String], zone: Tzdb.Entry.Zone): Tzdb.Entry.Zone =
      zone.copy(info = zone.info :+ parseZoneInfo(lineNo, args))

    @tailrec 
    def recur(lineNo: Int, lines: LazyList[String], entries: List[Tzdb.Entry], zone: Option[Tzdb.Entry.Zone]): List[Tzdb.Entry] =
      if lines.isEmpty then
        Log.fine(ansi"Finished parsing $lineNo lines of $name, and got ${entries.size} entries")
        entries ++ zone
      else
        val line = lines.head.takeWhile(_ != '#')
        line.rcut(r"\s+").to(List) match
          case "Rule" :: tail =>
            recur(lineNo + 1, lines.tail, parseRule(lineNo, tail) :: (zone.to(List) ++ entries), None)
          
          case "Link" :: tail =>
            recur(lineNo + 1, lines.tail, parseLink(lineNo, tail) :: (zone.to(List) ++ entries), None)
          
          case "Zone" :: tail =>
            recur(lineNo + 1, lines.tail, entries ++ zone.to(List), Some(parseZone(lineNo, tail)))
          
          case "Leap" :: tail =>
            recur(lineNo + 1, lines.tail, parseLeap(lineNo, tail) :: (zone.to(List) ++ entries), None)
          
          case "" :: Nil =>
            recur(lineNo + 1, lines.tail, entries, zone)
          
          case "" :: tail =>
            recur(lineNo + 1, lines.tail, entries, Some(addToZone(lineNo, tail, zone.getOrElse(throw TzParseError("Unexpected zoneinfo", lineNo)))))
          
          case other =>
            recur(lineNo + 1, lines.tail, entries, zone)

    recur(1, lines, Nil, None)

given realm: Realm = Realm("temporaneous")