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
package galilei

import java.nio.file as jnf

import anticipation.*
import contingency.*
import inimitable.*
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.Readable
import vacuous.*

import IoError.Operation

// `Platform` is the common base of galilei's OS filesystem platform types (`Posix`/`Linux`/`MacOs`/
// `Windows`/`Local`). It exists so that givens placed in its companion — notably the whole-file
// `Readable` instance — are in the implicit scope of every `Path on <platform>`, making
// `path.read` discoverable without an explicit import.
object Platform:
  given uuid: [uuid <: Uuid, filesystem <: Platform] => uuid is Admissible on filesystem =
    Admissible.unchecked[uuid, filesystem]

  // Read a path in its entirety as a single, direct operation: the whole file is read into memory
  // at once, holding no handle and needing no scope — unlike streaming a path, which must be
  // `open`ed and consumed within a scope. The whole `Data` is handed to a `Data is Readable to
  // result`, which decodes it directly when a direct instance exists (e.g. `Text`/`Data`) and
  // otherwise falls back to composing `Streamable` with `Aggregable`. Placing it here (rather than
  // as a `read` extension, which would be ambiguous with turbulence's generic one) makes
  // `path.read[…]` resolve through turbulence's `read` with no extra import for any `Path on …`.
  given pathReadable: [plane <: Platform: Filesystem, result]
  =>  ( readable: (Data is Readable to result)^ )
  =>  ( tactic: Tactic[IoError] )
  =>  (((Path on plane) is Readable to result)^{readable, tactic}) =
    path =>
      val bytes: Data = path.protect(Operation.Read):
        jnf.Files.readAllBytes(path.javaPath).nn.immutable(using Unsafe)

      readable.read(bytes)

// Pure: platforms are phantom plane markers, so capture checking never freshens them
// (keeping `=:=`-based plane unification exact through the `soundness` export aliases).
trait Platform extends caps.Pure
