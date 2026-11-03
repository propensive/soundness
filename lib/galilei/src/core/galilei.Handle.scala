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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import scala.caps

import anticipation.*
import aperture.*
import contingency.*
import prepositional.*
import turbulence.*
import zephyrine.*

object Handle:
  // A handle over whole-chunk endpoints alone — a `reader` that yields the whole content and
  // a `writer` that takes it — for a source that is not a file of this process: a client's
  // descriptor, carried to the daemon that serves it (`Fdtable`). The streaming
  // endpoints are spelled out rather than defaulted, as the WASI backend spells them: the
  // defaults' thunks carry root capabilities that are not visible from a downstream site.
  def whole(reader0: () -> Chain[Data], writer0: Chain[Data] -> Unit): Handle =
    new Handle:
      def reader()(using Tactic[Truncation.Error]): Chain[Data] = reader0()
      def writer(data: Chain[Data])(using Emit[Truncation.Error]): Unit = writer0(data)

      def source()(using Tactic[Truncation.Error]): (Stream[Data] over Credit)^ =
        Stream(reader0())

      def intake()(using Emit[Truncation.Error]): Intake[Data] over Credit =
        Sink.buffered((), (_, stream) => writer0(stream))

  // Polymorphic over the handle's (scoped, capturing) singleton type: `Openable.open`
  // hands the block a capability-refined `Handle^{...}`, and a `Self = Handle` instance
  // would not be summonable for it under capture checking. The `Granting` bounds gate each
  // typeclass by the grants selected when the handle was opened: a read-only handle is not
  // `Writable`, and the mismatch is a given-resolution failure at compile time.
  // Each instance retains the truncation tactic it is summoned under, through which a read or
  // write the file cuts short is raised.
  given streamable: [handle <: (Handle & Granting[Grant.Read])^]
  =>  ( tactic: Tactic[Truncation.Error] )
  =>  ( (handle is Streamable by Data over Credit)^{tactic} ) =
    _.source()

  given writable: [handle <: (Handle & Granting[Grant.Write])^]
  =>  ( tactic: Emit[Truncation.Error] )
  =>  ( (handle is Writable by Data)^{tactic} ) = (handle, stream) =>
    // The non-consume `write` crosses to the consuming pump as a neutral
    // reference (the `accept` convention).
    stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Data] over Credit)^]
    . pump(handle.intake().asInstanceOf[AnyRef].asInstanceOf[(Intake[Data] over Credit)^])

  given sink: [handle <: (Handle & Granting[Grant.Write])^]
  =>  ( tactic: Emit[Truncation.Error] )
  =>  ( (handle is Sink by Data over Credit)^{tactic} ) =
    // The intake crosses through an `AnyRef` rim, as `pump` above: it belongs to the
    // single-owner handle it is minted from.
    handle => handle.intake().asInstanceOf[AnyRef].asInstanceOf[(Intake[Data] over Credit)^]

  // The `read`/`write` operations are direct extensions as well as `Streamable`/`Writable`
  // typeclass givens, because summoning a typeclass on a *scoped* capability's refined type
  // can fail under capture checking (given resolution widens the scoped capture to `{any}`);
  // the extensions summon typeclasses on the (non-scoped) source/result types instead.
  extension (handle: (Handle & Granting[Grant.Read])^)
    def stream(using Tactic[Truncation.Error]): Chain[Data] = handle.reader()

    def read[result]
      ( using readable: (Chain[Data] is Readable to result)^, tactic: Tactic[Truncation.Error] )
    :   result =

      readable.read(handle.reader())

  extension (handle: (Handle & Granting[Grant.Write])^)
    def write[source](source: source)
      ( using streamable: (source is Streamable by Data over Credit)^, tactic: Emit[Truncation.Error] )
    :   Unit =

      streamable.stream(source)
      . pump(handle.intake().asInstanceOf[AnyRef].asInstanceOf[(Intake[Data] over Credit)^])

// An open file's endpoints: the whole content as a chain (`reader`/`writer`) and the streaming
// forms (`source`/`intake`) a backend serves from its own buffers. Each takes the truncation
// tactic at the call, since a read or write runs after the handle was made, under whatever
// tactic the caller has then.
abstract class Handle extends caps.ExclusiveCapability:
  def reader()(using Tactic[Truncation.Error]): Chain[Data]
  def writer(data: Chain[Data])(using Emit[Truncation.Error]): Unit
  def source()(using Tactic[Truncation.Error]): (Stream[Data] over Credit)^
  def intake()(using Emit[Truncation.Error]): Intake[Data] over Credit
