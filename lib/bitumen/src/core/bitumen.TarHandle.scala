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
package bitumen

import java.io as ji

import anticipation.*
import aperture.*
import contingency.*
import prepositional.*
import turbulence.*

// The scoped capability provided by opening an archive as `Tar`: `path.open[Tar]()`. TAR is a
// sequential format, so `entries` streams lazily from the underlying source (memoized by the
// `LazyList`, so revisiting an entry within the scope is free); payloads must be consumed
// within the scope, while the source remains open.
class TarHandle private[bitumen] (val entries: LazyList[Tar.Entry])
extends caps.ExclusiveCapability

// A named class rather than an anonymous given instance, for the reasons documented on
// galilei's `FileOpenable`. Archives open read-only: a `Write` mode is refused with
// `TarError.Reason.WriteUnsupported` until writing lands.
class TarOpenable[path: Abstractable across Paths to Text]
  ( using Tactic[TarError], Tactic[StreamError] )
extends Openable:

  type Self = path
  type Form = Tar
  type Operand = TarFlag
  type Result = TarHandle

  def open[grants <: Grant, result]
    ( value: path, mode: Mode granting grants, flags: List[TarFlag] )
    ( block: ((TarHandle & Granting[grants])^) ?=> result )
  :   result =

    if mode.atoms.contains(Write) then abort(TarError(TarError.Reason.WriteUnsupported))

    val in = ji.BufferedInputStream(ji.FileInputStream(value.generic.s))

    try
      val entries = TarHandle.entries(in.lazyList[Data], flags)
      block(using new TarHandle(entries) with Granting[grants] {})
    finally in.close()

class TarDataOpenable(using Tactic[TarError], Tactic[StreamError]) extends Openable:
  type Self = Data
  type Form = Tar
  type Operand = TarFlag
  type Result = TarHandle

  def open[grants <: Grant, result]
    ( value: Data, mode: Mode granting grants, flags: List[TarFlag] )
    ( block: ((TarHandle & Granting[grants])^) ?=> result )
  :   result =

    if mode.atoms.contains(Write) then abort(TarError(TarError.Reason.WriteUnsupported))
    val entries = TarHandle.entries(LazyList(value), flags)
    block(using new TarHandle(entries) with Granting[grants] {})

object TarHandle:
  private[bitumen] def entries(stream: LazyList[Data], flags: List[TarFlag])
    ( using Tactic[TarError], Tactic[StreamError] )
  :   LazyList[Tar.Entry] =

    flags.headOption match
      case Some(TarFlag.Gzip)    => Tarfile.fromGzip(stream)
      case Some(TarFlag.Zlib)    => Tarfile.fromZlib(stream)
      case Some(TarFlag.Deflate) => Tarfile.fromDeflate(stream)
      case None                  => Tarfile.read(stream)
