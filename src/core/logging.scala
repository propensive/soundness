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
import parasite.*, asyncOptions.cancelOrphans
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*

import language.experimental.pureFunctions

val dateFormat = jt.SimpleDateFormat(t"yyyy-MMM-dd HH:mm:ss.SSS".s)

infix type onto [Type <: { type Target }, TargetType] = Type { type Target = TargetType }

package logFormats:
  given Level is Showable as textLevel =
    case Level.Fine => t"[FINE]"
    case Level.Info => t"[INFO]"
    case Level.Warn => t"[WARN]"
    case Level.Fail => t"[FAIL]"

  given Message is Inscribable in Text as standard = (event, level, realm, timestamp) =>
    t"${dateFormat.format(timestamp)} $level ${realm.name.fit(10)} > $event\n"

  given Message is Inscribable in Text as untimestamped = (event, level, realm, timestamp) =>
    t"$level ${realm.name.fit(10)} > $event\n"

  given Message is Inscribable in Text as lightweight = (event, level, realm, timestamp) =>
    t"$level $event\n"

trait Inscribable:
  type Self
  type Format

  def formatter(message: Self, level: Level, realm: Realm, timestamp: Long): Format

  extension (message: Self) def format(level: Level, realm: Realm, timestamp: Long): Format =
    formatter(message, level, realm, timestamp)

def mute[FormatType](using erased DummyImplicit)[ResultType]
    (lambda: (FormatType is Loggable) ?=> ResultType)
        : ResultType =
  lambda(using Log.silent[FormatType])

trait Taggable:
  type Self
  type Operand

  extension (value: Self) def tag(tag: Operand): Self

extension (logObject: Log.type)
  def envelop[TagType, EventType: {Taggable by TagType, Loggable as loggable}](value: TagType)
      [ResultType]
      (lambda: (EventType is Loggable) ?=> ResultType)
          : ResultType =
    lambda(using loggable.contramap(_.tag(value)))

  def skip[EventType, MessageType]: EventType is Recordable into MessageType = new Recordable:
    type Self = EventType
    type Result = MessageType
    override def skip(event: EventType): Boolean = true
    def record(event: EventType): MessageType =
      throw Panic(msg"`skip` should prevent this from ever running")

  def silent[FormatType]: FormatType is Loggable = new Loggable:
    type Self = FormatType
    def log(level: Level, realm: Realm, timestamp: Long, event: FormatType): Unit = ()

  def apply[FormatType](using DummyImplicit)
      [EntryType: Inscribable in FormatType, TargetType: Appendable by FormatType]
      (target: TargetType)
      (using Monitor)
          : EntryType is Loggable =

    new:
      type Self = EntryType

      private lazy val funnel: Funnel[TargetType.Operand] =
        Funnel().tap: funnel =>
          val task = async(funnel.stream.appendTo(target))

          Hook.onShutdown:
            funnel.stop()
            unsafely(task.await())

      def log(level: Level, realm: Realm, timestamp: Long, event: EntryType): Unit =
        funnel.put(event.format(level, realm, timestamp))
