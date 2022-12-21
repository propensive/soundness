package temporaneous

import gossamer.*
import kaleidoscope.*
import eucalyptus.*
import rudiments.*

import scala.annotation.*
import unsafeExceptions.canThrowAny

object TzParseError:

  given Show[Issue] =
    case Issue.CouldNotParseTime(time) => t"Could not parse time $time"
    case Issue.UnexpectedRule          => t"Unexpected rule"
    case Issue.UnexpectedLink          => t"Unexpected link"
    case Issue.UnexpectedZoneInfo      => t"Unexpected zone info"
    case Issue.BadZoneInfo(line)       => t"Bad zone information: ${line.join(t"[", t"   ", t"]")}"
    case Issue.BadName(name)           => t"The name $name is not valid"

  enum Issue:
    case CouldNotParseTime(time: Text)
    case UnexpectedRule
    case UnexpectedLink
    case UnexpectedZoneInfo
    case BadZoneInfo(line: List[Text])
    case BadName(name: Text)

case class TzParseError(issue: TzParseError.Issue, line: Int)
extends Error(err"temporaneous: the timezone could not be parsed at line $line: $issue.show")

object Tzdb:
  case class Time(hours: Int, minutes: Int, seconds: Int, suffix: Maybe[Char])
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

  def parseFile(name: Text)(using Log): List[Tzdb.Entry] =
    val lines: LazyList[Text] =
      val stream = Option(getClass.getResourceAsStream(s"/temporaneous/$name")).getOrElse:
        throw Exception(s"could not find the file 'temporaneous/$name' on the classpath")
      .nn

      scala.io.Source.fromInputStream(stream).getLines.map(Text(_)).map(_.cut(t"\t").head.lower).to(LazyList)

    parse(name, lines)

  def parse(name: Text, lines: LazyList[Text])(using Log): List[Tzdb.Entry] =
    
    def parseDuration(lineNo: Int, str: Text) = str.cut(t":").to(List) match
      case h :: Nil           => Duration(h.as[Int], 0, 0)
      case h :: m :: Nil      => Duration(h.as[Int], m.as[Int], 0)
      case h :: m :: s :: Nil => Duration(h.as[Int], m.as[Int], s.as[Int])
      case other              => throw TzParseError(TzParseError.Issue.CouldNotParseTime(other.show), lineNo)

    def parseTime(lineNo: Int, str: Text) = str.cut(t":").to(List) match
      case As[Int](h) :: m :: Nil if m.ends(t"s")        => Time(h, m.drop(1, Rtl).as[Int], 0, 's')
      case As[Int](h) :: m :: Nil if m.ends(t"u")        => Time(h, m.drop(1, Rtl).as[Int], 0, 'u')
      case As[Int](h) :: As[Int](m) :: Nil               => Time(h, m, 0, Unset)
      case As[Int](h) :: As[Int](m) :: As[Int](s) :: Nil => Time(h, m, s, Unset)
      
      case other =>
        throw TzParseError(TzParseError.Issue.CouldNotParseTime(other.show), lineNo)

    def parseDay(month: MonthName, str: Text): MonthDate =
      if str.starts(t"last") then MonthDate.Last(month, Weekday.valueOf(str.drop(4).s))
      else if str.drop(3).take(2) == t">="
      then MonthDate.After(month, Weekday.valueOf(str.take(3).s), str.drop(5).as[Int])
      else if str.drop(3).take(2) == t"<="
      then MonthDate.Before(month, Weekday.valueOf(str.take(3).s), str.drop(5).as[Int])
      else MonthDate.Exact(month, str.as[Int])

    def parseLeap(lineNo: Int, args: List[Text]): Tzdb.Entry.Leap = args match
      case year :: month :: day :: time :: add :: s :: Nil =>
        Tzdb.Entry.Leap(year.as[Int], parseMonth(month), day.as[Int], parseTime(lineNo, time), add == t"+")

    def parseMonth(str: Text) = MonthName.valueOf(str.s)

    def parseZone(lineNo: Int, args: List[Text]): Tzdb.Entry.Zone = args match
      case name :: rest =>
        name.cut(t"/", 2).to(List) match
          case area :: location :: Nil =>
            Tzdb.Entry.Zone(area, Some(location), Vector(parseZoneInfo(lineNo, rest)))
          
          case simple :: Nil =>
            Tzdb.Entry.Zone(simple, None, Vector(parseZoneInfo(lineNo, rest)))
          
          case _ =>
            throw TzParseError(TzParseError.Issue.BadName(name), lineNo)
      case _                 =>
        throw TzParseError(TzParseError.Issue.UnexpectedRule, lineNo)

    def parseZoneInfo(lineNo: Int, args: List[Text]): Tzdb.ZoneInfo = args match
      case stdoff :: rules :: format :: until =>
        val s = parseDuration(lineNo, stdoff)
        
        def f(str: Text) = format.cut(t"%s", 2).to(List) match
          case value :: Nil           => value
          case before :: after :: Nil => before+str+after

        ZoneInfo(s, rules, f, if until.isEmpty then None else Some(until.join(t" ")))
      
      case other =>
        throw TzParseError(TzParseError.Issue.BadZoneInfo(other), lineNo)

    def parseLetters(str: Text): Option[Text] = if str == t"-" then None else Some(str)

    def parseRule(lineNo: Int, args: List[Text]): Tzdb.Entry.Rule = args match
      case name :: from :: to :: _ :: month :: day :: time :: save :: letters :: _ =>
        val end = to match
          case t"max"  => Int.MaxValue
          case t"only" => from.as[Int]
          case other   => to.as[Int]
        
        val d = parseDay(parseMonth(month), day)
        //val d = try parseDay(day) catch Exception => throw TzParseError("Couldn't parse day '"+day+"'", lineNo)
        val t = parseTime(lineNo, time)
        val s = parseDuration(lineNo, save)
        Tzdb.Entry.Rule(name, from.as[Int], end, d, t, s, parseLetters(letters))
      case _ =>
        throw TzParseError(TzParseError.Issue.UnexpectedRule, lineNo)

    def parseLink(lineNo: Int, args: List[Text]): Tzdb.Entry.Link = args match
      case from :: to :: Nil => Tzdb.Entry.Link(from, to)
      case _                 => throw TzParseError(TzParseError.Issue.UnexpectedLink, lineNo)

    def addToZone(lineNo: Int, args: List[Text], zone: Tzdb.Entry.Zone): Tzdb.Entry.Zone =
      zone.copy(info = zone.info :+ parseZoneInfo(lineNo, args))

    @tailrec 
    def recur(lineNo: Int, lines: LazyList[Text], entries: List[Tzdb.Entry], zone: Option[Tzdb.Entry.Zone]): List[Tzdb.Entry] =
      if lines.isEmpty then
        Log.fine(t"Finished parsing $lineNo lines of $name, and got ${entries.size} entries")
        entries ++ zone
      else
        val line: Text = lines.head.upto(_ != '#')
        line.cut(r"\s+").to(List) match
          case t"Rule" :: tail =>
            recur(lineNo + 1, lines.tail, parseRule(lineNo, tail) :: (zone.to(List) ++ entries), None)
          
          case t"Link" :: tail =>
            recur(lineNo + 1, lines.tail, parseLink(lineNo, tail) :: (zone.to(List) ++ entries), None)
          
          case t"Zone" :: tail =>
            recur(lineNo + 1, lines.tail, entries ++ zone.to(List), Some(parseZone(lineNo, tail)))
          
          case t"Leap" :: tail =>
            recur(lineNo + 1, lines.tail, parseLeap(lineNo, tail) :: (zone.to(List) ++ entries), None)
          
          case t"" :: Nil =>
            recur(lineNo + 1, lines.tail, entries, zone)
          
          case t"" :: tail =>
            recur(lineNo + 1, lines.tail, entries, Some(addToZone(lineNo, tail, zone.getOrElse:
              throw TzParseError(TzParseError.Issue.UnexpectedZoneInfo, lineNo))))
          
          case other =>
            recur(lineNo + 1, lines.tail, entries, zone)

    recur(1, lines, Nil, None)

given realm: Realm = Realm(t"temporaneous")