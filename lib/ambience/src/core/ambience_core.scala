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
package ambience

import scala.language.experimental.pureFunctions

import java.lang as jl

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

package systems:
  given emptySystem: System:
    def apply(name: Text): Unset.type = Unset

  given javaBaseSystem: System:
    def apply(name: Text): Optional[Text] = Optional(jl.System.getProperty(name.s)).let(_.tt)

package workingDirectories:
  // Derives the working directory from the ambient `System` (reading `user.dir`).
  given systemWorkingDirectory: (properties: System) => WorkingDirectory =
    () => properties(t"user.dir").or(panic(m"the property `user.dir` should be present"))

  // The `java.base` working directory: `systemWorkingDirectory` specialised to the JVM
  // `System`, equivalent to reading `user.dir` directly.
  given javaBaseWorkingDirectory: WorkingDirectory =
    systemWorkingDirectory(using ambience.systems.javaBaseSystem)

package environments:
  given emptyEnvironment: Environment:
    def variable(name: Text): Unset.type = Unset

  given javaBaseEnvironment: Environment:
    def variable(name: Text): Optional[Text] = Optional(jl.System.getenv(name.s)).let(_.tt)

package temporaryDirectories:
  given javaBaseTemporaryDirectory: TemporaryDirectory = () =>
    Optional(jl.System.getProperty("java.io.tmpdir")).let(_.tt).or:
      panic(m"the `java.io.tmpdir` system property is not set")

  given systemTemporaryDirectory: (system: System) => TemporaryDirectory =
    () => jl.System.getProperty("java.io.tmpdir").nn.tt

  given environmentTemporaryDirectory: Environment => TemporaryDirectory = () =>
    List("TMPDIR", "TMP", "TEMP").map(jl.System.getenv(_)).seek(_ != null)
    . let(_.nn.tt)
    . or(panic(m"none of `TMPDIR`, `TMP` or `TEMP` environment variables is set"))

// Resolution goes through `Paths.Resolver` (ordinary implicit search) rather than an inline
// `summonFrom`: the latter cannot be reduced when `temporaryDirectory`/`workingDirectory` is expanded
// inside a staged quote (e.g. an ethereal daemon `cli` block printing the working directory).
inline def temporaryDirectory[path: Representative of Paths]
  (using temporary: TemporaryDirectory, resolver: Paths.Resolver[path])
:   path =

  resolver(temporary.directory())


inline def workingDirectory[path: Representative of Paths]
  (using work: WorkingDirectory, resolver: Paths.Resolver[path])
:   path =

  resolver(work.directory())

package termcaps:
  given environmentTermcap: Environment => Termcap:
    val ansi: Boolean = true

    // The terminal width forwarded in `COLUMNS`, where the invoking context set it. When it is
    // absent or malformed — output redirected to a file or a pipe, say — the width stays
    // unbounded, and nothing wraps (#1789).
    override lazy val width: Int = safely(Environment.columns[Text].s.toInt).or(Int.MaxValue)

    lazy val color: ColorDepth =
      if safely(Environment.colorterm[Text]) == t"truecolor" then ColorDepth.TrueColor else
        val process = ProcessBuilder("tput", "colors").redirectErrorStream(true).nn.start().nn
        val output = process.getInputStream().nn.readAllBytes()

        if process.waitFor() != 0 then ColorDepth.NoColor
        else ColorDepth(String(output).trim.nn.toInt)
