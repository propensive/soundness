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

import proscenium.compat.*

import anticipation.*
import contingency.*
import galilei.*
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*
import zephyrine.*

import filesystemBackends.virtualMachine

// Opening a filesystem path or building an archive from disk needs `bitumen.jvm`; re-exported
// through `soundness.*`, so `path.open[Tar]` and `Tar.Entry(...)` resolve as before on the JVM.
given tarPathOpenable: [path: Abstractable across Paths to Text]
=>  ( tarTactic: Tactic[TarError], streamTactic: Tactic[StreamError] )
=>  ( TarOpenable[path]^{tarTactic, streamTactic} ) =
  TarOpenable[path]

given tarPathCreatable: [path: Abstractable across Paths to Text]
=>  (tactic: Tactic[TarError])
=>  ( TarBuilder.TarCreatable[path]^{tactic} ) =
  TarBuilder.TarCreatable[path]

extension (companion: Tarfile.type)
  // Build an archive from a directory tree on a filesystem.
  def from[plane <: Posix: Filesystem](root: Path on plane)
    ( using DereferenceSymlinks,
            TraversalOrder,
            plane is Explorable,
            Tactic[IoError],
            Tactic[TarError] )
  :   Tarfile =

    val entries: List[Tar.Entry] = root.descendants.to[List].map: path =>
      TarFilesystem.entryFor(root, path)

    Tarfile(entries)

extension (tarfile: Tarfile)
  // Extract an archive to a directory tree on a filesystem.
  def extractTo[plane <: Posix: Filesystem](root: Path on plane)
    ( using CreateNonexistentParents on plane,
            OverwritePreexisting on plane,
            Tactic[IoError],
            Tactic[TarError] )
  :   Unit =

    tarfile.entries.each: entry =>
      TarFilesystem.applyEntry(root, entry)
