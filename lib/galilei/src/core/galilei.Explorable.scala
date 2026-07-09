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

import java.nio.file as jnf

import anticipation.*
import contingency.*
import distillate.*
import prepositional.*
import serpentine.*
import spectacular.*

object Explorable:
  given Linux is Explorable:
    def children(path: Path on Linux): LazyList[Path on Linux] =
      given tactic: Tactic[PathError] = strategies.throwUnsafely

      if !jnf.Files.isDirectory(jnf.Path.of(path.show.s).nn) then LazyList() else
        jnf.Files.list(jnf.Path.of(path.show.s).nn).nn
        . iterator().nn
        . asScala
        . map(_.toString.tt.decode[Path on Linux])
        . to(LazyList)

  given Posix is Explorable:
    def children(path: Path on Posix): LazyList[Path on Posix] =
      given tactic: Tactic[PathError] = strategies.throwUnsafely

      if !jnf.Files.isDirectory(jnf.Path.of(path.show.s).nn) then LazyList() else
        jnf.Files.list(jnf.Path.of(path.show.s).nn).nn
        . iterator().nn
        . asScala
        . map(_.toString.tt.decode[Path on Posix])
        . to(LazyList)

  given Windows is Explorable:
    def children(path: Path on Windows): LazyList[Path on Windows] =
      given tactic: Tactic[PathError] = strategies.throwUnsafely

      if !jnf.Files.isDirectory(jnf.Path.of(path.show.s).nn) then LazyList() else
        jnf.Files.list(jnf.Path.of(path.show.s).nn).nn
        . iterator().nn
        . asScala
        . map(_.toString.tt.decode[Path on Windows])
        . to(LazyList)

  given MacOs is Explorable:
    def children(path: Path on MacOs): LazyList[Path on MacOs] =
      given tactic: Tactic[PathError] = strategies.throwUnsafely

      if !jnf.Files.isDirectory(jnf.Path.of(path.show.s).nn) then LazyList() else
        jnf.Files.list(jnf.Path.of(path.show.s).nn).nn
        . iterator().nn
        . asScala
        . map(_.toString.tt.decode[Path on MacOs])
        . to(LazyList)

  given local: ambience.System => Local is Explorable:
    def children(path: Path on Local): LazyList[Path on Local] =
      given tactic: Tactic[PathError] = strategies.throwUnsafely

      if !jnf.Files.isDirectory(jnf.Path.of(path.show.s).nn) then LazyList() else
        jnf.Files.list(jnf.Path.of(path.show.s).nn).nn
        . iterator().nn
        . asScala
        . map(_.toString.tt.decode[Path on Local])
        . to(LazyList)

trait Explorable extends Typeclass:
  def children(path: Path on Self): LazyList[Path on Self]
