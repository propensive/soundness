package temporaneous

import rudiments.*
import gossamer.*
import cardinality.*
import anticipation.*
import kaleidoscope.*

import math.Ordering.Implicits.infixOrderingOps

package calendars:
  given julian: RomanCalendar() with
    def leapYear(year: Y): Boolean = year%4 == 0
    def leapYearsSinceEpoch(year: Int): Int = year/4

  given gregorian: RomanCalendar() with
    def leapYear(year: Y): Boolean = year%4 == 0 && year%100 != 0 || year%400 == 0
    def leapYearsSinceEpoch(year: Int): Int = year/4 - year/100 + year/400 + 1


enum Weekday:
  case Mon, Tue, Wed, Thu, Fri, Sat, Sun

object Dates:
  opaque type Date = Int

  object Date:
    def of(day: Int): Date = day
    
    def apply(using cal: Calendar)(year: cal.Y, month: cal.M, day: cal.D): Date =
      cal.julianDay(year, month, day)

  given Ordering[Date] = Ordering.Int
  
  given Show[Date] = d =>
    given RomanCalendar = calendars.gregorian
    t"${d.day.toString.show}-${d.month.show}-${d.year.toString.show}"

  extension (date: Date)
    def day(using cal: Calendar): cal.D = cal.getDay(date)
    def month(using cal: Calendar): cal.M = cal.getMonth(date)
    def year(using cal: Calendar): cal.Y = cal.getYear(date)
    def yearDay(using cal: Calendar): Int = date - cal.zerothDayOfYear(cal.getYear(date))
    def julianDay: Int = date
    def at(time: Time)(using Calendar): Timestamp = Timestamp(date, time)
    
    @targetName("plus")
    def +(period: Timespan)(using cal: Calendar): Date = cal.add(date, period)

    @targetName("addDays")
    def +(days: Int): Date = date + days

export Dates.Date

trait Calendar:
  type D
  type M
  type Y

  def daysInYear(year: Y): Int

  def getYear(date: Date): Y
  def getMonth(date: Date): M
  def getDay(date: Date): D
  def zerothDayOfYear(year: Y): Date
  def julianDay(year: Y, month: M, day: D): Date
  def add(date: Date, period: Timespan): Date

abstract class RomanCalendar() extends Calendar:
  type Y = Int
  type M = MonthName
  type D = Int

  def leapYear(year: Y): Boolean

  def daysInMonth(month: M, year: Y): Int = month match
    case Jan | Mar | May | Jul | Aug | Oct | Dec => 31
    case Apr | Jun | Sep | Nov                   => 30
    case Feb                                     => if leapYear(year) then 29 else 28

  def add(date: Date, period: Timespan): Date =
    val monthTotal = getMonth(date).ordinal + period.months
    val month2 = MonthName.fromOrdinal(monthTotal%12)
    val year2 = getYear(date) + period.years + monthTotal/12
    julianDay(year2, month2, getDay(date)) + period.days
  
  def leapYearsSinceEpoch(year: Int): Int
  def daysInYear(year: Y): Int = if leapYear(year) then 366 else 365
  def zerothDayOfYear(year: Y): Date = Date.of(year*365 + leapYearsSinceEpoch(year) + 1721059)
  
  def getYear(date: Date): Int =
    def recur(year: Int): Int =
      val z = zerothDayOfYear(year).julianDay
      if z < date.julianDay && z + daysInYear(year) > date.julianDay then year else recur(year + 1)
    
    recur(((date.julianDay - 1721059)/366).toInt)
  
  def getMonth(date: Date): MonthName =
    val year = getYear(date)
    val ly = leapYear(year)
    MonthName.values.takeWhile(_.offset(ly) < date.yearDay(using this)).last
  
  def getDay(date: Date): Int =
    val year = getYear(date)
    val month = getMonth(date)
    date.julianDay - zerothDayOfYear(year).julianDay - month.offset(leapYear(year))
  
  def julianDay(year: Int, month: MonthName, day: Int): Date = 
    zerothDayOfYear(year) + month.offset(leapYear(year)) + day

class YearMonth[Y <: Int & Singleton, M <: MonthName & Singleton](year: Y, month: M):
  import compiletime.ops.int.*
  
  type CommonDays = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
      21 | 22 | 23 | 24 | 25 | 26 | 27 | 28

  type Days <: Int & Singleton = M match
    case Jan.type | Mar.type | May.type | Jul.type | Aug.type | Oct.type | Dec.type =>
      CommonDays | 29 | 30 | 31
    
    case Apr.type | Jun.type | Sep.type | Nov.type =>
      CommonDays | 29 | 30
    
    case Feb.type => Y%4 match
      case 0 => Y%100 match
        case 0 => Y%400 match
          case 0 => CommonDays | 29
          case _ => CommonDays
        case _ => CommonDays | 29
      case _ => CommonDays

  @targetName("of")
  inline def -(day: Days): Date = calendars.gregorian.julianDay(year, month, day)


extension (year: Int & Singleton)
  @targetName("of")
  inline def -(month: MonthName & Singleton): YearMonth[year.type, month.type] = new YearMonth(year, month)

object Timing:
  opaque type Instant = Long

  object Instant:
    given GenericInstant with
      type Instant = Timing.Instant
      def makeInstant(long: Long): Instant = long
      def readInstant(instant: Instant): Long = instant
    
    given GenericDuration with
      type Duration = Timing.Duration
      def makeDuration(long: Long): Duration = long
      def readDuration(duration: Duration): Long = duration

  opaque type Duration = Long

  extension (instant: Instant)
    @targetName("minus")
    def -(that: Instant): Duration = instant - that
    
    @targetName("plus")
    def +(that: Duration): Instant = instant + that
    
    @targetName("to")
    def ~(that: Instant): Interval = Interval(instant, that)

  extension (duration: Duration)
    @targetName("divide")
    def /(n: Int): Duration = duration/n
    
    @targetName("times")
    def *(n: Int): Duration = duration*n
    
    def from(instant: Instant): Interval = Interval(instant, instant + duration)

export Timing.{Instant, Duration}

case class Interval(from: Instant, to: Instant):
  def duration: Duration = to - from

trait Denomination

enum StandardTime extends Denomination:
  case Second, Minute, Hour, Day, Week, Month, Year

object TimeSystem:
  enum AmbiguousTimes:
    case Throw, Dilate, PreferEarlier, PreferLater
  
  enum MonthArithmetic:
    case Scale, Overcount, Fixed
  
  enum LeapDayArithmetic:
    case Throw, PreferFeb28, PreferMar1

open class TimeSystem[Units <: Denomination]():
  def ambiguousTimes: TimeSystem.AmbiguousTimes = TimeSystem.AmbiguousTimes.Dilate
  def monthArithmetic: TimeSystem.MonthArithmetic = TimeSystem.MonthArithmetic.Scale
  def leapDayArithmetic: TimeSystem.LeapDayArithmetic = TimeSystem.LeapDayArithmetic.PreferFeb28
  def simplify(period: Period): Period = period

given TimeSystem[StandardTime] with
  override def simplify(timespan: Timespan): Timespan =
    val timespan2 = if timespan.seconds < 60 then timespan else
      val adjust = timespan.seconds/60
      timespan + adjust.minutes - (adjust*60).seconds
    
    val timespan3 = if timespan2.minutes < 60 then timespan2 else
      val adjust = timespan2.minutes/60
      timespan2 + adjust.hours - (adjust*60).minutes
    
    val result = if timespan3.months < 12 then timespan3 else
      val adjust = timespan3.months/12
      timespan3 + adjust.years - (adjust*12).months
    
    result

type Timespan = Period

object Period:
  def apply(denomination: StandardTime, n: Int): Period = denomination match
    case StandardTime.Year   => Period(n, 0, 0, 0, 0, 0)
    case StandardTime.Month  => Period(0, n, 0, 0, 0, 0)
    case StandardTime.Day    => Period(0, 0, n, 0, 0, 0)
    case StandardTime.Hour   => Period(0, 0, 0, n, 0, 0)
    case StandardTime.Minute => Period(0, 0, 0, 0, n, 0)
    case StandardTime.Second => Period(0, 0, 0, 0, 0, n)

trait DiurnalPeriod:
  def years: Int
  def months: Int
  def days: Int

  

trait TemporalPeriod:
  def hours: Int
  def minutes: Int
  def seconds: Int

case class Period(override val years: Int, override val months: Int, override val days: Int, hours: Int,
                      minutes: Int, seconds: Int)
extends DiurnalPeriod, TemporalPeriod:
  @targetName("plus")
  def +(p: Period)(using timeSys: TimeSystem[StandardTime]): Period =
    Period(years + p.years, months + p.months, days + p.days, hours + p.hours, minutes + p.minutes, seconds +
        p.seconds)
  
  @targetName("minus")
  def -(p: Period)(using timeSys: TimeSystem[StandardTime]): Period =
    Period(years - p.years, months - p.months, days - p.days, hours - p.hours, minutes - p.minutes, seconds -
        p.seconds)
  
  def simplify(using timeSys: TimeSystem[StandardTime]): Period = timeSys.simplify(this)

  @targetName("times")
  def *(n: Int): Period = Period(years*n, months*n, days*n, hours*n, minutes*n, seconds*n)

extension (int: 1)
  def year: Timespan = Period(StandardTime.Year, 1)
  def month: Timespan = Period(StandardTime.Month, 1)
  def week: Timespan = Period(StandardTime.Week, 1)
  def day: Timespan = Period(StandardTime.Day, 1)
  def hour: Timespan = Period(StandardTime.Hour, 1)
  def minute: Timespan = Period(StandardTime.Minute, 1)
  def second: Timespan = Period(StandardTime.Second, 1)

extension (int: Int)
  def years: Timespan = Period(StandardTime.Year, int)
  def months: Timespan = Period(StandardTime.Month, int)
  def weeks: Timespan = Period(StandardTime.Week, int)
  def days: Timespan = Period(StandardTime.Day, int)
  def hours: Timespan = Period(StandardTime.Hour, int)
  def minutes: Timespan = Period(StandardTime.Minute, int)
  def seconds: Timespan = Period(StandardTime.Second, int)

enum WorldRegion:
  case Africa, Antarctica, Asia, Australasia, Etcetera, Europe, NorthAmerica, SouthAmerica

case class Locale(worldRegion: WorldRegion)
case class Timezone(name: Text, offset: Duration)
case class Time(hour: Base24, minute: Base60, second: Base60 = 0)

case class Timestamp(date: Date, time: Time)(using cal: Calendar):
  @targetName("plus")
  def +(period: Timespan): Timestamp =
    Timestamp(date, time)

enum MonthName:
  case Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
  
  def unapply(value: Text): Option[MonthName] =
    try Some(MonthName.valueOf(value.lower.capitalize.s)) catch case err: IllegalArgumentException => None
  
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

export MonthName.{Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec}

trait Clock:
  type Primary
  type Secondary
  type Tertiary
  type TimeRepr
  
  def addPrimary(time: Time, n: Primary): Time
  def addSecondary(time: Time, n: Secondary): Time
  def addTertiary(time: Time, n: Tertiary): Time

given sexagesimalClock: Clock with
  type Primary = Base24
  type Secondary = Base60
  type Tertiary = Base60
  type TimeRepr = Time

  def addPrimary(time: Time, n: Base24): Time = time.copy(hour = (time.hour + n)%%24)
  
  def addSecondary(time: Time, n: Base60): Time =
    val minute: Base60 = (time.minute + n)%%60
    val hour: Base24 = (time.hour + (time.minute + n)/60)%%24
    time.copy(hour = hour, minute = minute)
  
  def addTertiary(time: Time, n: Base60): Time =
    val second: Base60 = (time.second + n)%%60
    val minute: Base60 = (time.minute + (time.second + n)/60)%%60
    val hour: Base24 = (time.hour + (time.minute + (time.second + n)/60)/60)%%24
    Time(hour, minute, second)

type Base60 = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
                  21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 |
                  40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 |
                  59

type Base24 = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
                  21 | 22 | 23

given (using Unapply[Text, Int]): Unapply[Text, Base60] =
  case As[Int](value: Base60) => Some(value)
  case _                      => None

given (using Unapply[Text, Int]): Unapply[Text, Base24] =
  case As[Int](value: Base24) => Some(value)
  case _                      => None

extension (i: Int)
  @targetName("mod24")
  def %%(j: 24): Base24 =
    val x: Int = i%j.pipe { v => if v < 0 then v + 24 else v }
    x match
    case v: Base24 => v
    case _: Int    => throw Mistake("Modular arithmetic should produce value in range")
  
  @targetName("mod60")
  def %%(j: 60): Base60 =
    val x: Int = i%j.pipe { v => if v < 0 then v + 60 else v }
    x match
      case v: Base60 => v
      case _: Int    => throw Mistake("Modular arithmetic should produce value in range")

import compiletime.ops.double.{ToInt, + as ++, - as --, `*` as **}, compiletime.ops.int.*

@implicitNotFound("This is not a valid time in the form HH.MM.am or HH.MM.pm")
trait ValidTime[D <: Double & Singleton]

erased given [D <: Double & Singleton]
             (using ToInt[D] <= 12 =:= true, ToInt[D] > 0 =:= true,
                  ToInt[(D -- ToDouble[ToInt[D]]) ** 100.0] >= 0 =:= true,
                  ToInt[(D -- ToDouble[ToInt[D]]) ** 100.0] <= 59 =:= true): ValidTime[D] =
  compiletime.erasedValue

extension (double: Double)(using erased ValidTime[double.type])
  def am: Time =
    val hour: Base24 = (double.toInt%12).asInstanceOf[Base24]
    val minute: Base60 = ((double - double.toInt)*100.0 + 0.5).toInt.asInstanceOf[Base60]
    Time(hour, minute)
  
  def pm: Time =
    val hour: Base24 = ((double.toInt%12) + 12).toInt.asInstanceOf[Base24]
    val minute: Base60 = ((double - double.toInt)*100.0 + 0.5).toInt.asInstanceOf[Base60]
    Time(hour, minute)
