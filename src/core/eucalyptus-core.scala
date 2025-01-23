/*
    Eucalyptus, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import parasite.*, asyncTermination.cancel
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*

import language.experimental.pureFunctions

package logFormats:
  given textLevel: Level is Showable =
    case Level.Fine => t"FINE"
    case Level.Info => t"INFO"
    case Level.Warn => t"WARN"
    case Level.Fail => t"FAIL"

  given standard: [EventType: Communicable] => EventType is Inscribable in Text =
    (event, level, realm, timestamp) =>
      t"${dateFormat.format(timestamp).nn} [$level] ${realm.name.fit(10)} > ${event.communicate}\n"

  given untimestamped: [EventType: Communicable] => EventType is Inscribable in Text =
    (event, level, realm, timestamp) =>
      t"[$level] ${realm.name.fit(10)} > ${event.communicate}\n"

  given lightweight: [EventType: Communicable] => EventType is Inscribable in Text =
    (event, level, realm, timestamp) =>
      t"[$level] ${event.communicate}\n"

val dateFormat = jt.SimpleDateFormat(t"yyyy-MMM-dd HH:mm:ss.SSS".s)

def mute[FormatType](using erased DummyImplicit)[ResultType]
   (lambda: (FormatType is Loggable) ?=> ResultType)
:     ResultType =
  lambda(using Log.silent[FormatType])

extension (logObject: Log.type)
  def envelop[TagType, EventType: {Taggable by TagType, Loggable as loggable}](value: TagType)
     [ResultType]
     (lambda: (EventType is Loggable) ?=> ResultType)
  :     ResultType =
    lambda(using loggable.contramap(_.tag(value)))

  def ignore[EventType, MessageType]: MessageType transcribes EventType = new Transcribable:
    type Self = EventType
    type Result = MessageType
    override def skip(event: EventType): Boolean = true
    def record(event: EventType): MessageType =
      panic(m"`skip` should prevent this from ever running")

  def silent[FormatType]: FormatType is Loggable = new Loggable:
    type Self = FormatType
    def log(level: Level, realm: Realm, timestamp: Long, event: FormatType): Unit = ()

  def route[FormatType](using DummyImplicit)
     [EntryType: Inscribable in FormatType, TargetType: Writable by FormatType]
     (target: TargetType)
     (using Monitor)
  :     EntryType is Loggable =

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
  given silent: [FormatType] => FormatType is Loggable = Log.silent[FormatType]

  given stdout: [FormatType: Printable, EventType: Inscribable in FormatType] => Stdio
  =>    EventType is Loggable =
    (level, realm, timestamp, event) =>
      Out.println(EventType.formatter(event, level, realm, timestamp))

  given stderr: [EventType: Inscribable in FormatType, FormatType: Printable] => Stdio
  =>    EventType is Loggable =
    (level, realm, timestamp, event) =>
      Err.println(EventType.formatter(event, level, realm, timestamp))
