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

import turbulence.*
import parasite.*
import fulminate.*
import anticipation.*
import rudiments.*
import gossamer.*
import perforate.*

import java.text as jt

import scala.language.experimental.captureChecking

object Log:
  
  private val dateFormat = jt.SimpleDateFormat(t"yyyy-MMM-dd HH:mm:ss.SSS".s)

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

  inline def apply(inline routes: PartialFunction[Entry, Any])(using monitor: Monitor): Log =
    ${Eucalyptus.route('routes, 'monitor)}

@capability
abstract class Log():
  def envelopes: ListMap[Text, Text] = ListMap()
  
  def record(entry: Entry): Unit

  // def tag[ValueType](value: ValueType)(using envelope: Envelope[ValueType]): Log = new Log(actions):
  //   override def envelopes: ListMap[Text, Text] =
  //     thisLog.envelopes.updated(envelope.id, envelope.envelop(value))