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

import language.experimental.captureChecking

object Realm:
  given Show[Realm] = _.name
  def make(name: Text)(using Unsafe): Realm = Realm(name)

case class Realm private(name: Text):
  def unapply(entry: Entry): Boolean = entry.realm == this

object Level:
  given Ordering[Level] = Ordering[Int].on[Level](_.ordinal)
  given Communicable[Level] = level => Message(level.show.upper)
  given Show[Level] = _.toString.tt.upper

enum Level:
  case Fine, Info, Warn, Fail
  def unapply(entry: Entry): Boolean = entry.level == this
  
case class Entry(realm: Realm, level: Level, message: Message, timestamp: Long, envelopes: List[Text])

object Envelope:
  given [EnvelopeType](using Show[EnvelopeType]): Envelope[EnvelopeType] = _.show

trait Envelope[-EnvelopeType]:
  def envelope(value: EnvelopeType): Text

  given silent: Log = entry => ()

object Logger:
  val drain: Logger = stream => ()

  def apply
      [TargetType]
      (target: TargetType, appendable: Appendable[TargetType, Text], format: LogFormat[TargetType, Text])
      (using monitor: Monitor)
      : Logger^{monitor} =
    LiveLogger(target)(using appendable)

object LogWriter:
  given active
      [TargetType]
      (using appendable: Appendable[TargetType, Text], format: LogFormat[TargetType, Text], monitor: Monitor)
      : LogWriter[TargetType]^{monitor} =
    LiveLogger(_)(using appendable, format, monitor)

trait LogWriter[TargetType]:
  def logger(target: TargetType): Logger

trait Logger:
  def put(entry: Entry): Unit

class LiveLogger
    [TargetType]
    (target: TargetType)
    (using appendable: Appendable[TargetType, Text], format: LogFormat[TargetType, Text], monitor: Monitor)
extends Logger:
  private val funnel: Funnel[Entry] = Funnel()
  
  private val async: Async[Unit] = Async:
    appendable.append(target, unsafely(funnel.stream.map(format(_))))
  
  def put(entry: Entry): Unit = funnel.put(entry)

object LogFormat:
  given standard[TargetType]: LogFormat[TargetType, Text] = entry =>
    import textWidthCalculation.uniform
    val realm: Message = msg"${entry.realm.show.fit(8)}"
    msg"${Log.dateFormat.format(entry.timestamp).nn.tt} ${entry.level} $realm ${entry.message}".text+t"\n"
  
trait LogFormat[TargetType, +TextType]:
  def apply(entry: Entry): TextType

extension (inline context: StringContext)
  inline def realm(): Realm = ${Eucalyptus.realm('context)}

package logging:

  given pinned: Log = Log.pinned

  given stdout(using Stdio, Monitor): Log = Log.route:
    case _ => Out

  given stderr(using Stdio, Monitor): Log = Log.route:
    case _ => Err

  given silent: Log = new Log:
    def record(entry: Entry): Unit = ()
