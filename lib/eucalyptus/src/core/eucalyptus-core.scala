                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
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
import proscenium.*
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

  given standard: [event: Communicable] => event is Inscribable in Text =
    (event, level, realm, timestamp) =>
      t"${dateFormat.format(timestamp).nn} [$level] ${realm.name.fit(10)} > ${event.communicate}\n"

  given untimestamped: [event: Communicable] => event is Inscribable in Text =
    (event, level, realm, timestamp) =>
      t"[$level] ${realm.name.fit(10)} > ${event.communicate}\n"

  given lightweight: [event: Communicable] => event is Inscribable in Text =
    (event, level, realm, timestamp) =>
      t"[$level] ${event.communicate}\n"

val dateFormat = jt.SimpleDateFormat(t"yyyy-MMM-dd HH:mm:ss.SSS".s)

def mute[format](using erased Void)[result](lambda: (format is Loggable) ?=> result): result =
  lambda(using Log.silent[format])

extension (logObject: Log.type)
  def envelop[tag, event: {Taggable by tag, Loggable as loggable}](value: tag)[result]
       (lambda: (event is Loggable) ?=> result)
  :     result =
    lambda(using loggable.contramap(_.tag(value)))

  def ignore[event, message]: message transcribes event = new Transcribable:
    type Self = event
    type Result = message
    override def skip(event: event): Boolean = true
    def record(event: event): message = panic(m"`skip` should prevent this from ever running")

  def silent[format]: format is Loggable = new Loggable:
    type Self = format
    def log(level: Level, realm: Realm, timestamp: Long, event: format): Unit = ()

  def route[format](using erased Void)[entry: Inscribable in format, writable: Writable by format]
     (target: writable)
     (using Monitor)
  :     entry is Loggable =

    new:
      type Self = entry

      private lazy val spool: Spool[writable.Operand] = Spool().tap: spool =>
        val task = async(spool.stream.writeTo(target))

        System.intercept[Shutdown]:
          spool.stop()
          safely(task.await())

      def log(level: Level, realm: Realm, timestamp: Long, event: entry): Unit =
        spool.put(event.format(level, realm, timestamp))

package logging:
  given silent: [format] => format is Loggable = Log.silent[format]

  given stdout: [format: Printable, inscribable: Inscribable in format] => Stdio
        =>  inscribable is Loggable =
    (level, realm, timestamp, event) =>
      Out.println(inscribable.formatter(event, level, realm, timestamp))

  given stderr: [inscribable: Inscribable in format, format: Printable] => Stdio
        =>  inscribable is Loggable =
    (level, realm, timestamp, event) =>
      Err.println(inscribable.formatter(event, level, realm, timestamp))
