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
  given [TextType: Textual] => Log[TextType] is GenericLogger:
    type Self = Log[TextType]

    def logFine(log: Log[TextType], realm: Realm, message: => into Text): Unit =
      log.record(Entry(realm, Level.Fine, Textual(message), System.currentTimeMillis, Nil))

    def logInfo(log: Log[TextType], realm: Realm, message: => into Text): Unit =
      log.record(Entry(realm, Level.Info, Textual(message), System.currentTimeMillis, Nil))

    def logWarn(log: Log[TextType], realm: Realm, message: => into Text): Unit =
      log.record(Entry(realm, Level.Warn, Textual(message), System.currentTimeMillis, Nil))

    def logFail(log: Log[TextType], realm: Realm, message: => into Text): Unit =
      log.record(Entry(realm, Level.Fail, Textual(message), System.currentTimeMillis, Nil))

  val dateFormat = jt.SimpleDateFormat(t"yyyy-MMM-dd HH:mm:ss.SSS".s)

  given textLog[TextType](using log: Log[TextType])(using textual: Textual[TextType]): Log[Text] =
    log.contramap: text =>
      textual.make(text.s)

  inline def fine[MessageType](inline message: MessageType)[TextType]
      (using inline log: Log[TextType], inline realm: Realm, textual: Textual[TextType])
      (using inline show: textual.ShowType[MessageType])
          : Unit =

    ${Eucalyptus.record[MessageType, TextType]('{Level.Fine}, 'message, 'log, 'realm, 'textual, 'show)}

  inline def info[MessageType](inline message: MessageType)[TextType]
      (using inline log: Log[TextType], inline realm: Realm, textual: Textual[TextType])
      (using inline show: textual.ShowType[MessageType])
          : Unit =

    ${Eucalyptus.record[MessageType, TextType]('{Level.Info}, 'message, 'log, 'realm, 'textual, 'show)}

  inline def warn[MessageType](inline message: MessageType)[TextType]
      (using inline log: Log[TextType], inline realm: Realm, textual: Textual[TextType])
      (using inline show: textual.ShowType[MessageType])
          : Unit =

    ${Eucalyptus.record[MessageType, TextType]('{Level.Warn}, 'message, 'log, 'realm, 'textual, 'show)}

  inline def fail[MessageType](inline message: MessageType)[TextType]
      (using inline log: Log[TextType], inline realm: Realm, textual: Textual[TextType])
      (using inline show: textual.ShowType[MessageType])
          : Unit =

    ${Eucalyptus.record[MessageType, TextType]('{Level.Fail}, 'message, 'log, 'realm, 'textual, 'show)}

  inline def route[TextType](inline routes: PartialFunction[Entry[?], Any])(using monitor: Monitor)
          : Log[TextType] =

    ${Eucalyptus.route[TextType]('routes, 'monitor)}

  private val localLog: ThreadLocal[AnyRef] = ThreadLocal()

  inline def pin()(using inline log: Log[Text]): Unit = localLog.set(log)

  def pinned: Log[Text] = localLog.get() match
    case log: Log[Text] @unchecked => log
    case _                         => logging.silent

  def envelop[EnvelopeType: Envelope](value: EnvelopeType)[ResultType, TextType]
      (block: Log[TextType] ?=> ResultType)
      (using log: Log[TextType])
          : ResultType =

    val log2: Log[TextType] = new Log[TextType]:
      override val envelopes: List[Text] = summon[Envelope[EnvelopeType]].envelope(value) :: log.envelopes
      def record(entry: Entry[TextType]): Unit = log.record(entry)
    block(using log2)

@capability
abstract class Log[TextType]():
  private inline def outer: this.type = this
  val envelopes: List[Text] = Nil
  def record(entry: Entry[TextType]): Unit

  def contramap[TextType2](lambda: TextType2 => TextType): Log[TextType2] = new Log[TextType2]:
    def record(entry: Entry[TextType2]): Unit = outer.record(entry.map(lambda))
