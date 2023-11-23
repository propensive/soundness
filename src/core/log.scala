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

import parasite.*
import fulminate.*
import anticipation.*
import rudiments.*
import gossamer.*

import java.text as jt

//import scala.language.experimental.captureChecking

object Log:
  
  val dateFormat = jt.SimpleDateFormat(t"yyyy-MMM-dd HH:mm:ss.SSS".s)

  inline def fine
      [MessageType]
      (inline message: MessageType)
      [TextType]
      (using inline log: Log[TextType], inline realm: Realm)
      : Unit =
    ${Eucalyptus.record('{Level.Fine}, 'message, 'log, 'realm)}
  
  inline def info
      [MessageType]
      (inline message: MessageType)
      [TextType]
      (using inline log: Log[TextType], inline realm: Realm)
      : Unit =
    ${Eucalyptus.record('{Level.Info}, 'message, 'log, 'realm)}
  
  inline def warn
      [MessageType]
      (inline message: MessageType)
      [TextType]
      (using inline log: Log[TextType], inline realm: Realm)
      : Unit =
    ${Eucalyptus.record('{Level.Warn}, 'message, 'log, 'realm)}
  
  inline def fail
      [MessageType]
      (inline message: MessageType)
      [TextType]
      (using inline log: Log[TextType], inline realm: Realm)
      : Unit =
    ${Eucalyptus.record('{Level.Fail}, 'message, 'log, 'realm)}

  inline def route
      [TextType]
      (inline routes: PartialFunction[Entry[?], Any])
      (using monitor: Monitor)
      : Log[TextType] =
    ${Eucalyptus.route[TextType]('routes, 'monitor)}

  private val localLog: ThreadLocal[AnyRef] = ThreadLocal()
 
  inline def pin()(using inline log: Log[Text]): Unit = localLog.set(log)

  def pinned: Log[Text] = localLog.get() match
    case log: Log[Text] => log
    case _              => logging.silent

  def envelop
      [EnvelopeType: Envelope]
      (value: EnvelopeType)
      [ResultType, TextType]
      (block: Log[TextType] ?=> ResultType)
      (using log: Log[TextType])
      : ResultType =

    val log2: Log[TextType] = new Log[TextType]:
      override val envelopes: List[Text] = summon[Envelope[EnvelopeType]].envelope(value) :: log.envelopes
      def record(entry: Entry[TextType]): Unit = log.record(entry)
    block(using log2)

@capability
abstract class Log[TextType]():
  val envelopes: List[Text] = Nil
  def record(entry: Entry[TextType]): Unit
