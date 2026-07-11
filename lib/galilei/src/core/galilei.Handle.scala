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
package galilei

import anticipation.*
import contingency.*
import prepositional.*
import turbulence.*
import zephyrine.*

object Handle:
  // Polymorphic over the handle's (scoped, capturing) singleton type: `Openable.open`
  // hands the lambda a capability-refined `Handle^{...}`, and a `Self = Handle` instance
  // would not be summonable for it under capture checking.
  given streamable: [handle <: Handle^] => Tactic[StreamError]
  =>  handle is Streamable by Data = _.reader()

  given writable: [handle <: Handle^] => Emit[StreamError]
  =>  handle is Writable by Data = _.writer(_)

  given source: [handle <: Handle^] => handle is Source by Data over Credit = _.source()
  given sink: [handle <: Handle^] => handle is Sink by Data over Credit = _.intake()

// The native `source`/`intake` endpoints default to bridging the legacy
// `reader`/`writer`; `Openable` supplies channel-native endpoints, so file
// I/O reads and writes directly through the streaming kernel's buffers.
// A scoped capability: its `read`/`write` operations are direct methods as well as
// `Streamable`/`Writable` typeclass givens, because summoning a typeclass on a *scoped*
// capability's refined type can fail under capture checking (given resolution widens the
// scoped capture to `{any}`); the direct methods summon typeclasses on the (non-scoped)
// source/result types instead.
class Handle
  ( val reader: () => LazyList[Data], val writer: LazyList[Data] => Unit )
  ( val source: () => (Stream[Data] over Credit)^ = () => Stream(reader().iterator),
    val intake: () => Intake[Data] over Credit =
      () => Sink.buffered((), (_, stream) => writer(stream)) )
extends caps.ExclusiveCapability:

  def write[source: Streamable by Data as streamable](source: source): Unit =
    writer(streamable.stream(source))

  def stream: LazyList[Data] = reader()

  def read[result](using readable: (LazyList[Data] is Readable to result)^): result =
    readable.read(reader())
