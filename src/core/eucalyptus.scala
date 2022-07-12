/*
    Eucalyptus, version 0.4.0. Copyright 2018-22 Jon Pretty, Propensive OÜ.

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
import turbulence.*
import iridescence.*

import scala.quoted.*
import scala.collection.mutable.HashMap

import java.text as jt
import java.io as ji
import java.util as ju
import java.util.concurrent as juc

object Realm:
  given Show[Realm] = _.name
  given AnsiShow[Realm] = realm => ansi"${colors.LightGreen}(${realm.name})"

case class Realm(name: Text):
  def unapply(entry: Entry): Boolean = entry.realm == this

object Level:
  given Ordering[Level] = Ordering[Int].on[Level](_.ordinal)

  given AnsiShow[Level] = level =>
    val color = level match
      case Fine => solarized.Cyan
      case Info => solarized.Green
      case Warn => solarized.Yellow
      case Fail => solarized.Red

    ansi"${Bg(color)}[${colors.Black}($Bold( ${Showable(level).show.upper} ))]"

enum Level:
  case Fine, Info, Warn, Fail
  def unapply(entry: Entry): Boolean = entry.level == this
  
case class Entry(realm: Realm, level: Level, message: AnsiText, timestamp: Timestamp)

object Timestamp:
  def apply(): Timestamp = System.currentTimeMillis
  given show: Show[Timestamp] = ts => dateFormat.format(ju.Date(ts)).nn.show
  given AnsiShow[Timestamp] = timestamp => ansi"${colors.Tan}(${show.show(timestamp)})"

  private val dateFormat = jt.SimpleDateFormat(t"yyyy-MMM-dd HH:mm:ss.SSS".s)

opaque type Timestamp = Long

object ElapsedTime:
  def between(t0: Timestamp, current: Timestamp): ElapsedTime = (current - t0) max 0
  given show: Show[ElapsedTime] = ts => numberFormat.format(ts/1000.0).nn.show.pad(7)

  private val numberFormat = jt.DecimalFormat(t"#.000".s)

opaque type ElapsedTime = Long

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
      try $log.record(Entry($realm, $level, $show.ansiShow($value), ts))
      catch case e: Exception => ()
    }

case class Log(actions: PartialFunction[Entry, LogSink]*):
  private val funnels: HashMap[LogSink, Funnel[Entry]] = HashMap()
  
  private def put(target: LogSink, entry: Entry): Unit =
    if !funnels.contains(target) then
      synchronized:
        val funnel = Funnel[Entry]()
        Task(target.write(funnel.stream))
        funnels(target) = funnel

    funnels(target).put(entry)
  
  def record(entry: Entry): Unit = actions.flatMap(_.lift(entry)).foreach(put(_, entry))

package logging:
  given stdout: Log = Log(_ => SystemOut.sink)
  given silent: Log = Log()

object LogSink:
  def apply[S](sink: S, writable: Writable[S], format: LogFormat[S]): LogSink = new LogSink:
    type Sink = S
    def write(stream: LazyList[Entry]): Unit = unsafely(writable.write(sink, stream.map(format(_))))

trait LogSink:
  type Sink
  def write(stream: LazyList[Entry]): Unit 

object LogFormat:
  given standardAnsi: LogFormat[SystemOut.type] = entry =>
    ansi"${entry.timestamp.ansi} ${entry.level.ansi} ${entry.realm.ansi.span(8)} ${entry.message}".render.bytes
  
trait LogFormat[S]:
  def apply(entry: Entry): Bytes

extension [S: LogFormat: Writable](value: S)
  def sink: LogSink = LogSink(value, summon[Writable[S]], summon[LogFormat[S]])
