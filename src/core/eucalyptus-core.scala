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
import parasite.*, orphanDisposal.cancel
import rudiments.*
import spectacular.*
import prepositional.*
import turbulence.*

import language.experimental.pureFunctions

package logFormats:
  given Level is Showable as textLevel =
    case Level.Fine => t"FINE"
    case Level.Info => t"INFO"
    case Level.Warn => t"WARN"
    case Level.Fail => t"FAIL"

  given [EventType: Communicable] => EventType is Inscribable in Text as standard =
    (event, level, realm, timestamp) =>
      t"${dateFormat.format(timestamp).nn} [$level] ${realm.name.fit(10)} > ${event.communicate}\n"

  given [EventType: Communicable] => EventType is Inscribable in Text as untimestamped =
    (event, level, realm, timestamp) =>
      t"[$level] ${realm.name.fit(10)} > ${event.communicate}\n"

  given [EventType: Communicable] => EventType is Inscribable in Text as lightweight =
    (event, level, realm, timestamp) =>
      t"[$level] ${event.communicate}\n"

val dateFormat = jt.SimpleDateFormat(t"yyyy-MMM-dd HH:mm:ss.SSS".s)

def mute[FormatType](using erased DummyImplicit)[ResultType]
    (lambda: (FormatType is Loggable) ?=> ResultType)
        : ResultType =
  lambda(using Log.silent[FormatType])

extension (logObject: Log.type)
  def envelop[TagType, EventType: {Taggable by TagType, Loggable as loggable}](value: TagType)
      [ResultType]
      (lambda: (EventType is Loggable) ?=> ResultType)
          : ResultType =
    lambda(using loggable.contramap(_.tag(value)))

  def skip[EventType, MessageType]: MessageType transcribes EventType = new Transcribable:
    type Self = EventType
    type Result = MessageType
    override def skip(event: EventType): Boolean = true
    def record(event: EventType): MessageType =
      throw Panic(m"`skip` should prevent this from ever running")

  def silent[FormatType]: FormatType is Loggable = new Loggable:
    type Self = FormatType
    def log(level: Level, realm: Realm, timestamp: Long, event: FormatType): Unit = ()

  def route[FormatType](using DummyImplicit)
      [EntryType: Inscribable in FormatType, TargetType: Writable by FormatType]
      (target: TargetType)
      (using Monitor)
          : EntryType is Loggable =

    new:
      type Self = EntryType

      private lazy val spool: Spool[TargetType.Operand] =
        Spool().tap: spool =>
          val task = async(spool.stream.writeTo(target))

          Hook.onShutdown:
            spool.stop()
            unsafely(task.await())

      def log(level: Level, realm: Realm, timestamp: Long, event: EntryType): Unit =
        spool.put(event.format(level, realm, timestamp))

package logging:
  given [FormatType] => FormatType is Loggable as silent = Log.silent[FormatType]
  
  given [FormatType: Printable, EventType: Inscribable in FormatType](using Stdio)
      => EventType is Loggable as stdout =
    (level, realm, timestamp, event) =>
      Out.println(EventType.formatter(event, level, realm, timestamp))
  
  given [EventType: Inscribable in FormatType, FormatType: Printable](using Stdio)
      => EventType is Loggable as stderr =
    (level, realm, timestamp, event) =>
      Err.println(EventType.formatter(event, level, realm, timestamp))
