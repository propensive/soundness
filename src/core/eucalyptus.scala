/*
    Eucalyptus, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import rudiments.*
import fulminate.*
import anticipation.*
import perforate.*
import parasite.*
import turbulence.*
import spectacular.*
import hieroglyph.*

import scala.quoted.*

import java.text as jt
import java.util as ju

object Realm:
  given Show[Realm] = _.name
  def make(name: Text)(using Unsafe.type): Realm = Realm(name)

@missingContext("""|A contextual Realm is needed in scope. This is required for logging commands like `Log.info` and `Log.warn`, in order to tag them in log output. A realm can be specified with,
                     |    given Realm(t"project")
                     |typically at the top-level in a package called `project`. It is often useful to name the realm so that it can be referenced externally, for example when pattern matching on log messages, so,
                     |    given realm: Realm = Realm(t"project")
                     |may be more appropriate.""".stripMargin)
case class Realm private(name: Text):
  def unapply(entry: Entry): Boolean = entry.realm == this

object Level:
  given Ordering[Level] = Ordering[Int].on[Level](_.ordinal)
  given Communicable[Level] = level => msg"$level"

  // given Display[Level] = level =>
  //   val color = level match
  //     case Fine => solarized.Cyan
  //     case Info => solarized.Green
  //     case Warn => solarized.Yellow
  //     case Fail => solarized.Red

  //   e"${Bg(color)}[${colors.Black}($Bold( ${level.show.upper} ))]"

enum Level:
  case Fine, Info, Warn, Fail
  def unapply(entry: Entry): Boolean = entry.level == this
  
case class Entry(realm: Realm, level: Level, message: Message, timestamp: Timestamp, tags: ListMap[Text, Text])

object Timestamp:
  def apply(): Timestamp = System.currentTimeMillis
  given show: Show[Timestamp] = ts => dateFormat.format(ju.Date(ts)).nn.show
  given Communicable[Timestamp] = timestamp => msg"${timestamp}"

  private val dateFormat = jt.SimpleDateFormat(t"yyyy-MMM-dd HH:mm:ss.SSS".s)

opaque type Timestamp = Long

object Log:
  inline def fine
      [ValueType]
      (inline value: ValueType)
      (using inline log: Log, inline communicable: Communicable[ValueType], inline realm: Realm)
      : Unit =
    ${Eucalyptus.recordLog('{Level.Fine}, 'value, 'log, 'communicable, 'realm)}
  
  inline def info
      [ValueType]
      (inline value: ValueType)
      (using inline log: Log, inline communicable: Communicable[ValueType], inline realm: Realm)
      : Unit =
    ${Eucalyptus.recordLog('{Level.Info}, 'value, 'log, 'communicable, 'realm)}
  
  inline def warn
      [ValueType]
      (inline value: ValueType)
      (using inline log: Log, inline communicable: Communicable[ValueType], inline realm: Realm)
      : Unit =
    ${Eucalyptus.recordLog('{Level.Warn}, 'value, 'log, 'communicable, 'realm)}
  
  inline def fail
      [ValueType]
      (inline value: ValueType)
      (using inline log: Log, inline communicable: Communicable[ValueType], inline realm: Realm)
      : Unit =
    ${Eucalyptus.recordLog('{Level.Fail}, 'value, 'log, 'communicable, 'realm)}

object Eucalyptus:
  def recordLog
      [MessageType: Type]
      (level: Expr[Level], message: Expr[MessageType], log: Expr[Log],
          communicable: Expr[Communicable[MessageType]], realm: Expr[Realm])
      (using Quotes)
      : Expr[Unit] = '{
    val time = Timestamp()
    
    try $log.record(Entry($realm, $level, $communicable.message($message), time, $log.tags))
    catch case e: Exception => ()
  }

  def realm(context: Expr[StringContext])(using Quotes): Expr[Realm] =
    import quotes.reflect.*
    val name: String = context.valueOrAbort.parts.head
    if !name.matches("[a-z]+") then fail(msg"the realm name should comprise only of lowercase letters")
    else '{Realm.make(${Expr(name)}.tt)(using Unsafe)}

@missingContext("""|eucalyptus: a contextual Log instance is needed, for example:
                     |    import logging.stdout  // Log everything to standard output
                     |    import logging.silent  // Do not log anything""".stripMargin)
@capability
class Log(actions: PartialFunction[Entry, LogSink & Singleton]*)(using Monitor):
  transparent inline def thisLog = this
  def tags: ListMap[Text, Text] = ListMap()
  
  class Streamer(target: LogSink):
    lazy val funnel: Funnel[Entry] = Funnel()
    lazy val task: Async[Unit] = Async(target.write(unsafely(funnel.stream)))

  private val streamers: TrieMap[LogSink, Streamer] = TrieMap()
  
  private def put(target: LogSink, entry: Entry): Unit =
    streamers.putIfAbsent(target, Streamer(target))
    val streamer = streamers(target)
    streamer.funnel.put(entry)
    streamer.task
  
  def record(entry: Entry): Unit = actions.flatMap(_.lift(entry)).foreach(thisLog.put(_, entry))

  def tag[ValueType](value: ValueType)(using lt: LogTag[ValueType]): Log = new Log(actions*):
    override def tags: ListMap[Text, Text] = thisLog.tags.updated(lt.tagName, lt.tag(value))

trait LogTag[-TagType]:
  def tagName: Text
  def tag(value: TagType): Text

package logging:
  given stdout(using Stdio, Raises[StreamCutError], CharEncoder, Monitor): Log =
    val sink = Out.sink
  
    Log:
      case _ => sink

  given silent: Log =
    import errorHandlers.throwUnsafely
    supervise:
      Log { case _ => LogSink.drain }

object LogSink:
  val drain = new LogSink:
    def write(stream: LazyList[Entry]): Unit = ()

  def apply[SinkType](sink: SinkType, appendable: Appendable[SinkType, Text], format: LogFormat[SinkType]): LogSink = new LogSink:
    type Sink = SinkType
    def write(stream: LazyList[Entry]): Unit = unsafely(appendable.append(sink, stream.map(format(_))))

trait LogSink:
  type Sink
  def write(stream: LazyList[Entry]): Unit 

object LogFormat:
  given standardAnsi[SinkType]: LogFormat[SinkType] = entry =>
    import textWidthCalculation.uniform
    val realm: Message = msg"${entry.realm.show.fit(8)}"
    msg"${entry.timestamp} ${entry.level} $realm ${entry.message}".text
  
trait LogFormat[SinkType]:
  def apply(entry: Entry): Text

extension [SinkType: LogFormat](value: SinkType)
  def sink(using appendable: Appendable[SinkType, Text]): LogSink =
    LogSink(value, appendable, summon[LogFormat[SinkType]])

extension (inline context: StringContext)
  inline def realm(): Realm = ${Eucalyptus.realm('context)}
