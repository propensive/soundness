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
  given Communicable[Level] = level => Message(level.show.upper)

enum Level:
  case Fine, Info, Warn, Fail
  def unapply(entry: Entry): Boolean = entry.level == this
  
case class Entry(realm: Realm, level: Level, message: Message, timestamp: Long, envelopes: ListMap[Text, Text])

trait Envelope[-EnvelopeType]:
  def id: Text
  def envelop(value: EnvelopeType): Text

package logging:
  given stdout(using Stdio, Monitor): Log = Log:
    case _ => Out
  
  given silent: Log = entry => ()

object Logger:
  val drain: Logger = stream => ()

  def apply
      [TargetType]
      (target: TargetType, appendable: Appendable[TargetType, Text], format: LogFormat[TargetType])(using Monitor)
      : Logger =
    new ActiveLogger(target)(using appendable)


object LogWriter:
  given active
      [TargetType]
      (using appendable: Appendable[TargetType, Text], format: LogFormat[TargetType], monitor: Monitor)
      : LogWriter[TargetType] =
    ActiveLogger(_)(using appendable, format, monitor)

trait LogWriter[TargetType]:
  def logger(target: TargetType): Logger

trait Logger:
  def put(entry: Entry): Unit

class ActiveLogger
    [TargetType]
    (target: TargetType)
    (using appendable: Appendable[TargetType, Text], format: LogFormat[TargetType], monitor: Monitor)
extends Logger:
  private val funnel: Funnel[Entry] = Funnel()
  
  private val async: Async[Unit] = Async:
    appendable.append(target, unsafely(funnel.stream.map(format(_))))
  
  def put(entry: Entry): Unit =
    funnel.put(entry)

object LogFormat:
  given standardAnsi[TargetType]: LogFormat[TargetType] = entry =>
    import textWidthCalculation.uniform
    val realm: Message = msg"${entry.realm.show.fit(8)}"
    msg"${entry.timestamp} ${entry.level} $realm ${entry.message}".text+t"\n"
  
trait LogFormat[TargetType]:
  def apply(entry: Entry): Text

extension (inline context: StringContext)
  inline def realm(): Realm = ${Eucalyptus.realm('context)}