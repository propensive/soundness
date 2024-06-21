/*
    Eucalyptus, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import java.text as jt

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import hieroglyph.*, textMetrics.uniform
import parasite.*, asyncOptions.cancelOrphans
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*

import language.experimental.pureFunctions

val dateFormat = jt.SimpleDateFormat(t"yyyy-MMM-dd HH:mm:ss.SSS".s)

infix type onto [Type <: { type Target }, TargetType] = Type { type Target = TargetType }

package logFormats:
  given Level is Showable as textLevel =
    case Level.Fine => t"[FINE]"
    case Level.Info => t"[INFO]"
    case Level.Warn => t"[WARN]"
    case Level.Fail => t"[FAIL]"

  given Message is Inscribable in Text as standard = (level, realm, timestamp, message) =>
    t"${dateFormat.format(timestamp)} $level ${realm.name.fit(10)} > $message\n"

  given Message is Inscribable in Text as untimestamped = (level, realm, timestamp, message) =>
    t"$level ${realm.name.fit(10)} > $message\n"

  given Message is Inscribable in Text as lightweight = (level, realm, timestamp, message) =>
    t"$level $message\n"

trait Inscribable:
  type Self
  type Format
  def format(level: Level, realm: Realm, timestamp: Long, message: Self): Format

def mute[FormatType](using erased DummyImplicit)[ResultType]
    (lambda: (FormatType is Loggable) ?=> ResultType)
        : ResultType =
  lambda(using Log.mute[FormatType])

extension (logObject: Log.type)

  def skip[EventType, MessageType]: EventType is Recordable into MessageType = new Recordable:
    type Self = EventType
    type Result = MessageType
    override def skip(event: EventType): Boolean = true
    def record(event: EventType): MessageType =
      throw Panic(msg"`skip` should prevent this from ever running")

  def mute[FormatType]: FormatType is Loggable = new Loggable:
    type Self = FormatType
    def log(level: Level, realm: Realm, timestamp: Long, event: FormatType): Unit = ()

  def apply[FormatType](using DummyImplicit)
      [EntryType: Inscribable in FormatType, TargetType: Appendable by FormatType]
      (target: TargetType)
      (using Monitor)
          : EntryType is Loggable =

    new:
      type Self = EntryType

      private lazy val funnel: Funnel[TargetType.Operand] =
        Funnel().tap: funnel =>
          val task = async(funnel.stream.appendTo(target))

          Hook.onShutdown:
            funnel.stop()
            unsafely(task.await())

      def log(level: Level, realm: Realm, timestamp: Long, event: EntryType): Unit =
        funnel.put(EntryType.format(level, realm, timestamp, event))


/*
case class Entry[TextType]
    (realm: Realm, level: Level, message: TextType, timestamp: Long, envelopes: List[Text]):

  def map[TextType2](lambda: TextType => TextType2): Entry[TextType2] =
    Entry(realm, level, lambda(message), timestamp, envelopes)

object Envelope:
  given [EnvelopeType: Showable]: Envelope[EnvelopeType] = _.show

trait Envelope[-EnvelopeType]:
  def envelope(value: EnvelopeType): Text

object Logger:
  def drain[AnyType]: Logger[AnyType] = stream => ()

  def apply[TargetType: Appendable by TextType, TextType]
      (target: TargetType, : LogFormat[TargetType, TextType])
      (using Monitor)
          : Logger[TextType]/*^{monitor}*/ =

    LogProcess(target)(using format)

object LogWriter:
  given active[TargetType: Appendable by TextType, TextType](using format: LogFormat[TargetType, TextType])
      (using monitor: Monitor)
          : LogWriter[TargetType, TextType]/*^{monitor}*/ =

    LogProcess[TargetType, TextType](_)(using format)

trait LogWriter[TargetType, TextType]:
  def logger(target: TargetType): Logger[TextType]

trait Logger[TextType]:
  def put(entry: Entry[TextType]): Unit

class LogProcess[TargetType: Appendable by TextType, TextType](target: TargetType)
    (using format: LogFormat[TargetType, TextType])
    (using monitor: Monitor)
extends Logger[TextType]:

  private val funnel: Funnel[Entry[TextType]] = Funnel()

  private val task: Daemon =
    daemon(TargetType.append(target, unsafely(funnel.stream.map(format(_)))))

  def put(entry: Entry[TextType]): Unit = funnel.put(entry)

object LogFormat:
  given standard[TargetType]: LogFormat[TargetType, Text] = entry =>
    import textMetrics.uniform
    val realm: Text = entry.realm.name.fit(8)
    t"${Log.dateFormat.format(entry.timestamp).nn.tt} ${entry.level} $realm ${entry.message}\n"

trait LogFormat[TargetType, TextType]:
  def apply(entry: Entry[TextType]): TextType

package logging:
  given pinned: SimpleLogger = Log.pinned

  given stdout(using Stdio, Monitor, Text is Presentational): Log[Text] = Log.route[Text]:
    case _ => Out

  given stderr(using Stdio, Monitor, Text is Presentational): Log[Text] = Log.route[Text]:
    case _ => Err

  given SimpleLogger as silent:
    def logFine(realm: Realm, message: => Text): Unit = ()
    def logInfo(realm: Realm, message: => Text): Unit = ()
    def logWarn(realm: Realm, message: => Text): Unit = ()
    def logFail(realm: Realm, message: => Text): Unit = ()
*/
