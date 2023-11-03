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
  given Communicable[Level] = level => msg"$level"

enum Level:
  case Fine, Info, Warn, Fail
  def unapply(entry: Entry): Boolean = entry.level == this
  
case class Entry(realm: Realm, level: Level, message: Message, timestamp: Long, envelopes: ListMap[Text, Text])

object Eucalyptus:
  def recordLog
      [MessageType: Type]
      (level: Expr[Level], message: Expr[MessageType], log: Expr[Log],
          communicable: Expr[Communicable[MessageType]], realm: Expr[Realm])
      (using Quotes)
      : Expr[Unit] =
  '{
    val time = System.currentTimeMillis
    
    try $log.record(Entry($realm, $level, $communicable.message($message), time, $log.envelopes))
    catch case e: Exception => ()
  }

  def realm(context: Expr[StringContext])(using Quotes): Expr[Realm] =
    import quotes.reflect.*
    val name: String = context.valueOrAbort.parts.head
    if !name.matches("[a-z]+") then fail(msg"the realm name should comprise only of lowercase letters")
    else '{Realm.make(${Expr(name)}.tt)(using Unsafe)}

trait Envelope[-EnvelopeType]:
  def id: Text
  def envelop(value: EnvelopeType): Text

package logging:
  given stdout(using Stdio, CharEncoder, Monitor): Log =
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
