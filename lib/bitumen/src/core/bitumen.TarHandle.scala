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
package bitumen

import anticipation.*
import aperture.*
import contingency.*
import prepositional.*
import pneumatic.*
import turbulence.*
import zephyrine.*

// The scoped capability provided by opening an archive as `Tar`: `path.open[Tar]()`. TAR is a
// sequential format, so `entries` parses lazily from the underlying source, one entry per
// step; payloads must be consumed within the scope, while the source remains open. The
// iterator is single-pass: an entry passed over remains readable (its body memoizes when the
// iterator advances), but the sequence itself is not replayable within the scope.
class TarHandle private[bitumen] (entries0: Iterator[Tar.Entry]^)
extends caps.ExclusiveCapability:

  // Reached only through this exclusive handle, which scopes it; its capture
  // of the underlying source is erased here, as the memoizing `LazyList` it
  // replaces erased it implicitly through its pure cells.
  @caps.unsafe.untrackedCaptures
  val entries: Iterator[Tar.Entry] = caps.unsafe.unsafeAssumePure(entries0)

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
    val entries = TarHandle.entries(value.stream, flags)
    block(using new TarHandle(entries) with Granting[grants] {})

object TarHandle:
  private[bitumen] def entries(consume stream: (Stream[Data] over Credit)^, flags: List[TarFlag])
    ( using Tactic[TarError], Tactic[StreamError], Buffering )
  :   Iterator[Tar.Entry]^ =

    Tarfile.read:
      flags.headOption match
        case Some(TarFlag.Gzip)    => stream.decompress[Gzip]
        case Some(TarFlag.Zlib)    => stream.decompress[Zlib]
        case Some(TarFlag.Deflate) => stream.decompress[Deflate]
        case _                     => stream
