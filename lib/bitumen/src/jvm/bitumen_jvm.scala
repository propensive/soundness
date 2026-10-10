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


import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import galilei.*
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*
import zephyrine.*

import filesystemBackends.javaBaseFilesystem

// Opening a filesystem path as an archive, or creating one on disk, needs `bitumen.jvm`;
// re-exported through `soundness.*`, so `path.open[Tar]` and `path.create[Tar]` resolve as
// before on the JVM. Archiving a directory (`directory.archive[Tar]()`) goes through galilei's
// filesystem backend and lives in `bitumen.core`.
given tarPathOpenable: [path: Abstractable across Paths to Text]
=>  ( tarTactic: Tactic[Tar.Error], streamTactic: Tactic[Truncation.Error] )
=>  ( TarOpenable[path]^{tarTactic, streamTactic} ) =
  TarOpenable[path]

given arPathOpenable: [path: Abstractable across Paths to Text]
=>  ( arTactic: Tactic[Ar.Error], streamTactic: Tactic[Truncation.Error] )
=>  ( ArOpenable[path]^{arTactic, streamTactic} ) =
  ArOpenable[path]

given tarPathCreatable: [path: Abstractable across Paths to Text]
=>  (tactic: Tactic[Tar.Error])
=>  ( TarBuilder.TarCreatable[path]^{tactic} ) =
  TarBuilder.TarCreatable[path]

extension (tarfile: Tarfile)
  // Extract an archive to a directory tree on a filesystem.
  def extractTo[plane <: Posix: Filesystem](root: Path on plane)
    ( using CreateNonexistentParents on plane,
            OverwritePreexisting on plane,
            Tactic[Io.Error],
            Tactic[Tar.Error] )
  :   Unit =

    val created: scm.HashSet[java.nio.file.Path] = scm.HashSet()

    tarfile.entries.each: entry =>
      TarFilesystem.applyEntry(root, entry, created)
