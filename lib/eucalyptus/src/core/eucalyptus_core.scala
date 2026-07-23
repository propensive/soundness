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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.language.experimental.pureFunctions

import java.text as jt

import anticipation.*
import fulminate.*
import gossamer.*
import prepositional.*
import spectacular.*

package logFormats:
  given textLevelLogFormat: Level is Showable =
    case Level.Fine => t"FINE"
    case Level.Info => t"INFO"
    case Level.Warn => t"WARN"
    case Level.Fail => t"FAIL"

  given standardLogFormat: [event: Communicable] => event is Inscribable in Text =
    (event, level, timestamp) =>
      t"${dateFormat.format(timestamp).nn} [$level] ${event.communicate}\n"

  given untimestampedLogFormat: [event: Communicable] => event is Inscribable in Text =
    (event, level, timestamp) =>
      t"[$level] ${event.communicate}\n"

  given lightweightLogFormat: [event: Communicable] => event is Inscribable in Text =
    (event, level, timestamp) =>
      t"[$level] ${event.communicate}\n"

val dateFormat = jt.SimpleDateFormat(t"yyyy-MMM-dd HH:mm:ss.SSS".s)

// Runs `lambda` with logging of `event` suppressed, regardless of any ambient `LogSink`s. The
// silent
// `event is Loggable` is supplied directly to the block, so (as a lexically-scoped given) it takes
// precedence over the companion-scoped `Loggable.fanOut`.
def mute[event](using erased void: Void)[result](lambda: (event is Loggable) ?=> result): result =
  val silent: event is Loggable = new Loggable:
    type Self = event
    def log(level: Level, timestamp: Long, event: => event): Unit = ()

  lambda(using silent)

package logging:
  // A silent logger; imported explicitly (`import logging.silentLogging`) it outranks the
  // companion-scoped `Loggable.fanOut`, suppressing all logging for the file.
  given silentLogging: [event] => event is Loggable = new Loggable:
    type Self = event
    def log(level: Level, timestamp: Long, event: => event): Unit = ()
