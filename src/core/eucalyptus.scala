package eucalyptus

import gossamer.*
import escapade.*
import iridescence.*

import scala.annotation.*

import java.text as jt
import java.util as ju

object Timestamp:
  def apply(): Timestamp = System.currentTimeMillis
  given Highlight[Timestamp] = _ => colors.Tan

  private val dateFormat = jt.SimpleDateFormat("yyyy-MMM-dd HH:mm:ss.SSS")

  given Show[Timestamp] = ts => dateFormat.format(ju.Date(ts)).nn.text

opaque type Timestamp = Long

object Level:
  given AnsiShow[Level] = level =>
    val color = level match
      case Fine => colors.DarkGreen
      case Info => colors.SteelBlue
      case Warn => colors.Goldenrod
      case Fail => colors.OrangeRed

    ansi"${Bg(color)}[${colors.Black}($Bold( ${level.toString.upper} ))]"

enum Level:
  case Fine, Info, Warn, Fail

object Log:
  def fine[T](value: T)(using Log, AnsiShow[T], Realm): Unit =
    val ts = Timestamp()
    summon[Log].log(Entry(summon[Realm], Level.Fine, value.ansi, ts))
  
  def info[T](value: T)(using Log, AnsiShow[T], Realm): Unit =
    val ts = Timestamp()
    summon[Log].log(Entry(summon[Realm], Level.Info, value.ansi, ts))
  
  def warn[T](value: T)(using Log, AnsiShow[T], Realm): Unit =
    val ts = Timestamp()
    summon[Log].log(Entry(summon[Realm], Level.Warn, value.ansi, ts))
  
  def fail[T](value: T)(using Log, AnsiShow[T], Realm): Unit =
    val ts = Timestamp()
    summon[Log].log(Entry(summon[Realm], Level.Fail, value.ansi, ts))

  def stdout(format: LogFormat = LogFormat.standard): Log = new Log:
    def log(entry: Entry): Unit =
      println(format.format(entry).render)
  
  def silent: Log = new Log:
    def log(entry: Entry): Unit = ()

@implicitNotFound("eucalyptus: a contextual Log is required, for example:\n    given Log = Log.stdout()\nor,\n    given Log = Log.silent")  
trait Log:
  def log(entry: Entry): Unit

object Realm:
  given Show[Realm] = _.name.text
  given Highlight[Realm] = _ => colors.LightGreen

case class Realm(name: String)

case class Entry(realm: Realm, level: Level, message: AnsiString, timestamp: Timestamp)

object LogFormat:
  val standard: LogFormat = entry =>
    ansi"${entry.timestamp.ansi} ${entry.level.ansi} ${entry.realm.ansi.span(10)} ${entry.message}"

trait LogFormat:
  def format(entry: Entry): AnsiString