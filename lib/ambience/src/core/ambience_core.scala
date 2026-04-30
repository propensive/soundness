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
package ambience

import language.experimental.pureFunctions

import java.lang as jl
import java.nio.file as jnf

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

package systems:
  given empty: System:
    def apply(name: Text): Unset.type = Unset

  given java: System:
    def apply(name: Text): Optional[Text] = Optional(jl.System.getProperty(name.s)).let(_.tt)

package workingDirectories:
  given system: (properties: System) => WorkingDirectory =
    () => properties(t"user.dir").or(panic(m"the property `user.dir` should be present"))

  given java: WorkingDirectory = system(using ambience.systems.java)

  given system: WorkingDirectory = () =>
    Optional(jl.System.getProperty("user.dir")).let(_.tt).or:
      panic(m"the `user.dir` system property is not set")

  given default: WorkingDirectory = () => jnf.Paths.get("").nn.toAbsolutePath.toString

package environments:
  given empty: Environment:
    def variable(name: Text): Unset.type = Unset

  given java: Environment:
    def variable(name: Text): Optional[Text] = Optional(jl.System.getenv(name.s)).let(_.tt)

package temporaryDirectories:
  given java: TemporaryDirectory = () =>
    Optional(jl.System.getProperty("java.io.tmpdir")).let(_.tt).or:
      panic(m"the `java.io.tmpdir` system property is not set")

  given system: (system: System) => TemporaryDirectory =
    () => jl.System.getProperty("java.io.tmpdir").nn.tt

  given environment: Environment => TemporaryDirectory = () =>
    List("TMPDIR", "TMP", "TEMP").map(jl.System.getenv(_)).map(Optional(_)).compact.prim.let(_.tt)
    . or(panic(m"none of `TMPDIR`, `TMP` or `TEMP` environment variables is set"))


inline def temporaryDirectory[path: Representative of Paths](using temporary: TemporaryDirectory)
:   path =

  compiletime.summonFrom:
    case given (`path` is Instantiable across Paths from Paths.Trusted) =>
      Paths.Trusted(temporary.directory()).instantiate

    case given (`path` is Instantiable across Paths from Text) =>
      temporary.directory().instantiate


inline def workingDirectory[path: Representative of Paths](using work: WorkingDirectory): path =
  compiletime.summonFrom:
    case given (`path` is Instantiable across Paths from Paths.Trusted) =>
      Paths.Trusted(work.directory()).instantiate

    case given (`path` is Instantiable across Paths from Text) =>
      work.directory().instantiate

package termcaps:
  given environment: Environment => Termcap:
    val ansi: Boolean = true

    lazy val color: ColorDepth =
      if safely(Environment.colorterm[Text]) == t"truecolor" then ColorDepth.TrueColor else
        val process = ProcessBuilder("tput", "colors").redirectErrorStream(true).nn.start().nn
        val output = process.getInputStream().nn.readAllBytes()

        if process.waitFor() != 0 then ColorDepth.NoColor
        else ColorDepth(String(output).trim.nn.toInt)
