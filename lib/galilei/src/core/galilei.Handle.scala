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
import aperture.*
import contingency.*
import prepositional.*
import turbulence.*
import zephyrine.*

object Handle:
  // Polymorphic over the handle's (scoped, capturing) singleton type: `Openable.open`
  // hands the block a capability-refined `Handle^{...}`, and a `Self = Handle` instance
  // would not be summonable for it under capture checking. The `Granting` bounds gate each
  // typeclass by the grants selected when the handle was opened: a read-only handle is not
  // `Writable`, and the mismatch is a given-resolution failure at compile time.
  given streamable: [handle <: (Handle & Granting[Grant.Read])^]
  =>  handle is Streamable by Data over Credit = _.source()

  given writable: [handle <: (Handle & Granting[Grant.Write])^]
  =>  handle is Writable by Data = (handle, stream) =>
    // The non-consume `write` crosses to the consuming pump as a neutral
    // reference (the `accept` convention).
    stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Data] over Credit)^]
    . pump(handle.intake().asInstanceOf[AnyRef].asInstanceOf[(Intake[Data] over Credit)^])

  given sink: [handle <: (Handle & Granting[Grant.Write])^]
  =>  handle is Sink by Data over Credit = _.intake()

  // The `read`/`write` operations are direct extensions as well as `Streamable`/`Writable`
  // typeclass givens, because summoning a typeclass on a *scoped* capability's refined type
  // can fail under capture checking (given resolution widens the scoped capture to `{any}`);
  // the extensions summon typeclasses on the (non-scoped) source/result types instead.
  extension (handle: (Handle & Granting[Grant.Read])^)
    def stream: LazyList[Data] = handle.reader()

    def read[result](using readable: (LazyList[Data] is Readable to result)^): result =
      readable.read(handle.reader())

  extension (handle: (Handle & Granting[Grant.Write])^)
    def write[source](source: source)
      ( using streamable: (source is Streamable by Data over Credit)^ )
    :   Unit =
      streamable.stream(source)
      . pump(handle.intake().asInstanceOf[AnyRef].asInstanceOf[(Intake[Data] over Credit)^])

// The native `source`/`intake` endpoints default to bridging the legacy
// `reader`/`writer`; `FileOpenable` supplies channel-native endpoints, so file
// I/O reads and writes directly through the streaming kernel's buffers.
class Handle
  ( val reader: () => LazyList[Data], val writer: LazyList[Data] => Unit )
  ( val source: Spring[Data]^ = () => Stream(reader().iterator),
    val intake: () => Intake[Data] over Credit =
      () => Sink.buffered((), (_, stream) => writer(stream)) )
extends caps.ExclusiveCapability
