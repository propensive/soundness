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

import parasite.*
import anticipation.*
import rudiments.*
import gossamer.*

import java.text as jt

import scala.language.experimental.pureFunctions
import scala.language.experimental.into

object Log:
  val dateFormat = jt.SimpleDateFormat(t"yyyy-MMM-dd HH:mm:ss.SSS".s)

  given textLog[TextType: Presentational](using log: Log[TextType]): Log[Text] = log.contramap(TextType(_))

  inline def fine[MessageType](inline message: MessageType)[TextType]
      (using inline log: Log[TextType], inline realm: Realm, presentational: TextType is Presentational)
      (using inline show: presentational.Show[MessageType])
          : Unit =

    ${Eucalyptus.record[MessageType, TextType]('{Level.Fine}, 'message, 'log, 'realm, 'presentational, 'show)}

  inline def info[MessageType](inline message: MessageType)[TextType]
      (using inline log: Log[TextType], inline realm: Realm, presentational: TextType is Presentational)
      (using inline show: presentational.Show[MessageType])
          : Unit =

    ${Eucalyptus.record[MessageType, TextType]('{Level.Info}, 'message, 'log, 'realm, 'presentational, 'show)}

  inline def warn[MessageType](inline message: MessageType)[TextType]
      (using inline log: Log[TextType], inline realm: Realm, presentational: TextType is Presentational)
      (using inline show: presentational.Show[MessageType])
          : Unit =

    ${Eucalyptus.record[MessageType, TextType]('{Level.Warn}, 'message, 'log, 'realm, 'presentational, 'show)}

  inline def fail[MessageType](inline message: MessageType)[TextType]
      (using inline log: Log[TextType], inline realm: Realm, presentational: TextType is Presentational)
      (using inline show: presentational.Show[MessageType])
          : Unit =

    ${Eucalyptus.record[MessageType, TextType]('{Level.Fail}, 'message, 'log, 'realm, 'presentational, 'show)}

  inline def route[TextType](inline routes: PartialFunction[Entry[?], Any])
      (using monitor: Monitor)
      (using inline presentational: TextType is Presentational)
          : Log[TextType] =

    ${Eucalyptus.route[TextType]('routes, 'monitor, 'presentational)}

  private val localLog: ThreadLocal[AnyRef] = ThreadLocal()

  inline def pin()(using inline log: SimpleLogger): Unit = localLog.set(log)

  def pinned: SimpleLogger = localLog.get() match
    case log: SimpleLogger => log
    case _                 => logging.silent

  def envelop[EnvelopeType: Envelope](value: EnvelopeType)[ResultType, TextType: Presentational]
      (block: Log[TextType] ?=> ResultType)
      (using log: Log[TextType])
          : ResultType =

    val log2: Log[TextType] = new Log[TextType]:
      override val envelopes: List[Text] = summon[Envelope[EnvelopeType]].envelope(value) :: log.envelopes
      def record(entry: Entry[TextType]): Unit = log.record(entry)
      def logFine(realm: Realm, message: => Text): Unit = log.logFine(realm, message)
      def logInfo(realm: Realm, message: => Text): Unit = log.logInfo(realm, message)
      def logWarn(realm: Realm, message: => Text): Unit = log.logWarn(realm, message)
      def logFail(realm: Realm, message: => Text): Unit = log.logFail(realm, message)

    block(using log2)

@capability
abstract class Log[MessageType]() extends SimpleLogger:
  private inline def outer: this.type = this

  val envelopes: List[Text] = Nil
  def record(entry: Entry[MessageType]): Unit

  def contramap[MessageType2](lambda: MessageType2 => MessageType): Log[MessageType2] =
    new Log[MessageType2]:
      def logFine(realm: Realm, message: => Text): Unit = outer.logFine(realm, message)
      def logInfo(realm: Realm, message: => Text): Unit = outer.logInfo(realm, message)
      def logWarn(realm: Realm, message: => Text): Unit = outer.logWarn(realm, message)
      def logFail(realm: Realm, message: => Text): Unit = outer.logFail(realm, message)
      def record(entry: Entry[MessageType2]): Unit = outer.record(entry.map(lambda))

abstract class TextLog[MessageType](present: Text => MessageType) extends Log[MessageType]:
  def logFine(realm: Realm, message: => Text): Unit =
    record(Entry(realm, Level.Fine, present(message), System.currentTimeMillis, Nil))

  def logInfo(realm: Realm, message: => Text): Unit =
    record(Entry(realm, Level.Info, present(message), System.currentTimeMillis, Nil))

  def logWarn(realm: Realm, message: => Text): Unit =
    record(Entry(realm, Level.Warn, present(message), System.currentTimeMillis, Nil))

  def logFail(realm: Realm, message: => Text): Unit =
    record(Entry(realm, Level.Fail, present(message), System.currentTimeMillis, Nil))
