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
┃    Soundness, version 0.46.0.                                                                    ┃
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

import anticipation.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

package systems:
  given empty: System:
    def apply(name: Text): Unset.type = Unset

  given jre: System:
    def apply(name: Text): Optional[Text] = Optional(System.getProperty(name.s)).let(_.tt)

package workingDirectories:
  given system: (properties: System) => WorkingDirectory =
    () => properties(t"user.dir").or(panic(m"the property `user.dir` should be present"))

  given jre: WorkingDirectory = system(using ambience.systems.jre)

  given system: WorkingDirectory = () =>
    Optional(System.getProperty("user.dir")).let(_.tt).or:
      panic(m"the `user.dir` system property is not set")

  given default: WorkingDirectory = () => java.nio.file.Paths.get("").nn.toAbsolutePath.toString

package homeDirectories:
  given system: (properties: System) => HomeDirectory =
    () => properties(t"user.home").or(panic(m"the property `user.home` should be present"))

  given jre: HomeDirectory = system(using ambience.systems.jre)

  given system: HomeDirectory = () =>
    Optional(System.getProperty("user.home")).let(_.tt).or:
      panic(m"the `user.home` system property is not set")

  given environment: HomeDirectory = () =>
    List("HOME", "USERPROFILE", "HOMEPATH").map(System.getenv(_)).map(Optional(_)).compact.prim
    . let(_.tt)
    . or(panic(m"none of `HOME`, `USERPROFILE` or `HOMEPATH` environment variables is set"))


package environments:
  given empty: Environment:
    def variable(name: Text): Unset.type = Unset

  given jre: Environment:
    def variable(name: Text): Optional[Text] = Optional(System.getenv(name.s)).let(_.tt)

package temporaryDirectories:
  given system: TemporaryDirectory = () =>
    Optional(System.getProperty("java.io.tmpdir")).let(_.tt).or:
      panic(m"the `java.io.tmpdir` system property is not set")

  given environment: TemporaryDirectory = () =>
    List("TMPDIR", "TMP", "TEMP").map(System.getenv(_)).map(Optional(_)).compact.prim.let(_.tt).or:
      panic(m"none of `TMPDIR`, `TMP` or `TEMP` environment variables is set")


inline def temporaryDirectory[path: Representative of Paths](using temporary: TemporaryDirectory)
: path =

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

def homeDirectory[path: Instantiable across Paths from Text](using directory: HomeDirectory): path =
  directory.path[path]
