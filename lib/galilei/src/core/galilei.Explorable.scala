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
┃    Soundness, version 0.43.0.                                                                    ┃
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
import proscenium.*
import serpentine.*
import spectacular.*

object Explorable:
  given Linux is Explorable:
    def children(path: Path on Linux): Stream[Path on Linux] =
      given tactic: Tactic[PathError] = strategies.throwUnsafely

      if !jnf.Files.isDirectory(jnf.Path.of(path.show.s).nn) then Stream() else
        jnf.Files.list(jnf.Path.of(path.show.s).nn).nn
        . iterator().nn
        . asScala
        . map(_.toString.tt.decode[Path on Linux])
        . to(Stream)

  given Windows is Explorable:
    def children(path: Path on Windows): Stream[Path on Windows] =
      given tactic: Tactic[PathError] = strategies.throwUnsafely

      if !jnf.Files.isDirectory(jnf.Path.of(path.show.s).nn) then Stream() else
        jnf.Files.list(jnf.Path.of(path.show.s).nn).nn
        . iterator().nn
        . asScala
        . map(_.toString.tt.decode[Path on Windows])
        . to(Stream)

  given MacOs is Explorable:
    def children(path: Path on MacOs): Stream[Path on MacOs] =
      given tactic: Tactic[PathError] = strategies.throwUnsafely

      if !jnf.Files.isDirectory(jnf.Path.of(path.show.s).nn) then Stream() else
        jnf.Files.list(jnf.Path.of(path.show.s).nn).nn
        . iterator().nn
        . asScala
        . map(_.toString.tt.decode[Path on MacOs])
        . to(Stream)

trait Explorable extends Typeclass:
  def children(path: Path on Self): Stream[Path on Self]
