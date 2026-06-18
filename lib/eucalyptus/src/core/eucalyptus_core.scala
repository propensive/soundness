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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import language.experimental.pureFunctions

import java.text as jt

import anticipation.*
import frontier.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
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

// Derives the single `event is Loggable` that `Log.fine`/`info`/`warn`/`fail` summon by collecting
// EVERY in-scope `Logger` for the event (contravariance means a `Logger[Any, Message]` is picked up
// for any event), transcribing the event to the common `Message` carrier once, then fanning out to
// each logger. No logger in scope ⇒ an empty `Every` ⇒ silent; many ⇒ fan-out + per-logger routing.
given fanOut: [event]
=>  ( every:        Every[Logger[event, Message]],
      transcribable: event is Transcribable to Message )
=>  event is Loggable =

  (level, timestamp, event) =>
    if !transcribable.skip(event) then
      val message = transcribable.record(event)
      every.values.each(_.submit(level, timestamp, message))

// Runs `lambda` with logging of `event` suppressed, regardless of any ambient `Logger`s. The silent
// `event is Loggable` is supplied directly to the block, so (as an explicitly-passed given) it
// takes precedence over the wildcard `fanOut`.
def mute[event](using erased Void)[result](lambda: (event is Loggable) ?=> result): result =
  val silent: event is Loggable = (level, timestamp, event) => ()
  lambda(using silent)

package logging:
  // A silent logger; imported explicitly (`import logging.silentLogging`) it outranks the wildcard
  // `fanOut`, suppressing all logging for the file.
  given silentLogging: [event] => event is Loggable = (level, timestamp, event) => ()
