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

import gossamer.*
import rudiments.*
import anticipation.*
import contingency.*
import parasite.*, asyncOptions.cancelOrphans
import turbulence.*
import spectacular.*
import hieroglyph.*

import language.experimental.pureFunctions

object Level:
  given Ordering[Level] = Ordering[Int].on[Level](_.ordinal)
  given Show[Level] = _.toString.tt.upper

enum Level:
  case Fine, Info, Warn, Fail
  def unapply(entry: Entry[?]): Boolean = entry.level == this

case class Entry[TextType]
    (realm: Realm, level: Level, message: TextType, timestamp: Long, envelopes: List[Text]):

  def map[TextType2](lambda: TextType => TextType2): Entry[TextType2] =
    Entry(realm, level, lambda(message), timestamp, envelopes)

object Envelope:
  given [EnvelopeType](using Show[EnvelopeType]): Envelope[EnvelopeType] = _.show

trait Envelope[-EnvelopeType]:
  def envelope(value: EnvelopeType): Text

object Logger:
  def drain[AnyType]: Logger[AnyType] = stream => ()

  def apply[TargetType: Appendable by TextType, TextType]
      (target: TargetType, format: LogFormat[TargetType, TextType])
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
