/*
    Eucalyptus, version 0.2.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package eucalyptus

import gossamer.*
import escapade.*
import rudiments.*
import iridescence.*

import scala.annotation.*
import scala.quoted.*

import java.text as jt
import java.io as ji
import java.util as ju
import java.util.concurrent as juc

object Timestamp:
  def apply(): Timestamp = System.currentTimeMillis
  given show: Show[Timestamp] = ts => dateFormat.format(ju.Date(ts)).nn.show
  given AnsiShow[Timestamp] = timestamp => ansi"${colors.Tan}(${show.show(timestamp)})"

  private val dateFormat = jt.SimpleDateFormat(t"yyyy-MMM-dd HH:mm:ss.SSS".s)


opaque type Timestamp = Long

opaque type ElapsedTime = Long

object ElapsedTime:
  def between(t0: Timestamp, current: Timestamp): ElapsedTime = (current - t0) max 0
  given show: Show[ElapsedTime] = ts => numberFormat.format(ts/1000.0).nn.show.padLeft(7)

  private val numberFormat = jt.DecimalFormat(t"#.000".s)

object Level:
  given AnsiShow[Level] = level =>
    val color = level match
      case Fine => solarized.Cyan
      case Info => solarized.Green
      case Warn => solarized.Yellow
      case Fail => solarized.Red

    ansi"${Bg(color)}[${colors.Black}($Bold( ${Showable(level).show.upper} ))]"

enum Level:
  case Fine, Info, Warn, Fail

  @targetName("greaterThan")
  def >(level: Level): Boolean = this.ordinal > level.ordinal
  
  @targetName("lessThan")
  def <(level: Level): Boolean = this.ordinal < level.ordinal
  
  @targetName("greaterThanOrEqualTo")
  def >=(level: Level): Boolean = this.ordinal >= level.ordinal
  
  @targetName("lessThanOrEqualTo")
  def <=(level: Level): Boolean = this.ordinal <= level.ordinal

object Log:
  inline def fine[T](inline value: T)
                    (using inline log: Log, inline show: AnsiShow[T], inline realm: Realm): Unit =
    ${recordLog('{Level.Fine}, 'value, 'log, 'show, 'realm)}
  
  inline def info[T](inline value: T)
                    (using inline log: Log, inline show: AnsiShow[T], inline realm: Realm): Unit =
    ${recordLog('{Level.Info}, 'value, 'log, 'show, 'realm)}
  
  inline def warn[T](inline value: T)
                    (using inline log: Log, inline show: AnsiShow[T], inline realm: Realm): Unit =
    ${recordLog('{Level.Warn}, 'value, 'log, 'show, 'realm)}
  
  inline def fail[T](inline value: T)
                    (using inline log: Log, inline show: AnsiShow[T], inline realm: Realm): Unit =
    ${recordLog('{Level.Fail}, 'value, 'log, 'show, 'realm)}

  private def recordLog[T: Type](level: Expr[Level], value: Expr[T], log: Expr[Log],
                                     show: Expr[AnsiShow[T]], realm: Expr[Realm])
                                (using Quotes): Expr[Unit] =
    import quotes.reflect.*

    '{
      val ts = Timestamp()
      try
        if $log.interested($realm, $level)
        then $log.record(Entry($realm, $level, $show.ansiShow($value), ts))
      catch case e: Exception => ()
    }

  def silent: Log = Log()

object Everything extends Realm(t"")

@implicitNotFound("eucalyptus: a given Log instance is required, for example:\n    import euc"+
    "alyptus.*\n    given Log(Everything |-> Stdout)\nor,\n    import eucalyptus.*\n    given Log "+
    "= Log.silent")
case class Log(rules: Rule[?, ?]*):

  class DistributedLog(format: LogFormat[?, ?], interested: Entry => Boolean,
                           loggers: Iterable[Logger]):
    def record(entry: Entry): Unit =
      if interested(entry) then loggers.foreach(_.record(format.serialize(format.format(entry))))


  lazy val loggers: Iterable[DistributedLog] =
    rules.groupBy(_.format).map:
      (format, rules) =>
        DistributedLog(
          format,
          entry => rules.exists(_.interested(entry.realm, entry.level)),
          rules.groupBy(_.dest).map:
            (dest, rules) => Logger(rules.head.writer, rules, format.interval)
          .map(_.start())
        )

  def record(entry: Entry): Unit = loggers.foreach(_.record(entry))
  def interested(realm: Realm, level: Level): Boolean = rules.exists(_.interested(realm, level))

case class Rule[S, T](realm: Realm, level: Level, format: LogFormat[S, T], dest: S, sink: Sink[S]):
  def writer(stream: LazyList[Bytes]): Unit =
    try sink.write(dest, stream.map(identity(_))) catch case e: Exception => ()

  def interested(realm: Realm, level: Level): Boolean =
    (this.realm == Everything || realm == this.realm) && level >= this.level

object Realm:
  given Show[Realm] = _.name
  given AnsiShow[Realm] = realm => ansi"${colors.LightGreen}(${realm.name})"

case class Realm(name: Text):

  inline def realm: this.type = this

  @targetName("directTo")
  def |->[D: Sink, T](dest: D, level: Level = Level.Fine)(using fmt: LogFormat[D, T]): Rule[D, T] =
    Rule(realm, level, fmt, dest, summon[Sink[D]])
  
  object fine:
    @targetName("directTo")
    def |->[S: Sink, T](sink: S)(using LogFormat[S, T]): Rule[S, T] = realm.|->(sink, Level.Fine)

  object info:
    @targetName("directTo")
    def |->[S: Sink, T](sink: S)(using LogFormat[S, T]): Rule[S, T] = realm.|->(sink, Level.Info)

  object warn:
    @targetName("directTo")
    def |->[S: Sink, T](sink: S)(using LogFormat[S, T]): Rule[S, T] = realm.|->(sink, Level.Warn)

  object fail:
    @targetName("directTo")
    def |->[S: Sink, T](sink: S)(using LogFormat[S, T]): Rule[S, T] = realm.|->(sink, Level.Fail)

case class Entry(realm: Realm, level: Level, message: AnsiString, timestamp: Timestamp)

object LogFormat:
  val t0 = Timestamp()
  given LogFormat[Stdout.type, AnsiString] with
    override def interval: Int = 50
    def serialize(value: AnsiString): Bytes = value.render.bytes

    def format(entry: Entry): AnsiString =
      ansi"${entry.timestamp.ansi} ${entry.level.ansi} ${entry.realm.ansi.span(8)} ${entry.message}"

  val timed: LogFormat[Stdout.type, AnsiString] = new LogFormat[Stdout.type, AnsiString]:
    override def interval: Int = 50
    def serialize(value: AnsiString): Bytes = value.render.bytes
    def format(entry: Entry): AnsiString =
      ansi"${ElapsedTime.between(t0, entry.timestamp)} ${entry.level.ansi} ${entry.realm.ansi.span(8)} ${entry.message}"

trait LogFormat[S, T]:
  def interval: Int = 500
  def format(entry: Entry): T
  def serialize(value: T): Bytes

abstract class PlainLogFormat[S] extends LogFormat[S, String]:
  def serialize(value: String): Bytes = value.show.bytes

abstract class AnsiLogFormat[S] extends LogFormat[S, AnsiString]:
  def serialize(value: AnsiString): Bytes = value.render.bytes

object Logger:
  private var threadId: Int = -1
  
  private def run(runnable: Runnable): Unit =
    val id = synchronized:
      threadId += 1
      threadId
    
    val name = t"eucalyptus-$id"

    val thread = Thread(runnable, name.s)
    thread.start()

class Logger(writer: LazyList[Bytes] => Unit, rules: Seq[Rule[?, ?]], interval: Int):
  private val queue: juc.ConcurrentLinkedQueue[Bytes] = juc.ConcurrentLinkedQueue[Bytes]()
  private val parentThread: Thread = Thread.currentThread.nn
  private val buf = ji.ByteArrayOutputStream()
  private val newline = t"\n".bytes.unsafeMutable

  def record(entry: Bytes): Unit = queue.offer(entry)

  def logStream: LazyList[IArray[Byte]] =
    Thread.sleep(interval)
    buf.reset()
    while !queue.isEmpty do
      buf.write(queue.poll.nn.unsafeMutable)
      buf.write(newline)

    buf.toByteArray.nn.unsafeImmutable #:: (if parentThread.isAlive then logStream else LazyList())

  def start(): Logger =
    Logger.run:
      () => try writer(logStream) catch case e: Exception => ()
    this

given realm: Realm = Realm(t"eucalyptus")