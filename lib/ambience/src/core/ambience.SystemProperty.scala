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
┃    Soundness, version 0.32.0.                                                                    ┃
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

import language.dynamics

import scala.compiletime.ops.string.*

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import proscenium.*
import vacuous.*

trait SystemProperty[name <: String, property]:
  def read(value: Text): property

object SystemProperty:
  given generic: [unknown <: String & Singleton] => (erased Void)
        =>  SystemProperty[unknown, Text] =
    identity(_)

  given javaHome: [path: Instantiable across Paths from Text]
        =>  SystemProperty["java.home", path] =
    path(_)

  given javaLibraryPath: [path: Instantiable across Paths from Text]
        => (systemProperties: SystemProperties, systemProperty: Tactic[SystemPropertyError])
        =>  SystemProperty["java.library.path", List[path]] =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(path(_))

  given javaClassPath: [path: Instantiable across Paths from Text]
        => (systemProperties: SystemProperties, systemProperty: Tactic[SystemPropertyError])
        =>  SystemProperty["java.class.path", List[path]] =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(path(_))

  given javaVersion: SystemProperty["java.version", Text] = identity(_)
  given javaRuntimeVersion: SystemProperty["java.runtime.version", Text] = identity(_)

  given javaExtDirs: [path: Instantiable across Paths from Text]
        => (systemProperties: SystemProperties, systemProperty: Tactic[SystemPropertyError])
        =>  SystemProperty["java.ext.dirs", List[path]] =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(path(_))

  given fileSeparator: SystemProperty["file.separator", Char] = _.decode[Char]
  given pathSeparator: SystemProperty["path.separator", Char] = _.decode[Char]
  given lineSeparator: SystemProperty["line.separator", Text] = identity(_)

  given userName: SystemProperty["user.name", Text] = identity(_)

  given userHome: [path: Instantiable across Paths from Text]
        =>  SystemProperty["user.home", path] =
    path(_)

  given userDir: [path: Instantiable across Paths from Text]
        =>  SystemProperty["user.dir", path] =
    path(_)

  given osName: SystemProperty["os.name", Text] = identity(_)
  given osVersion: SystemProperty["os.version", Text] = identity(_)
  given osArch: SystemProperty["os.arch", Text] = identity(_)

  given decoder: [unknown <: Label, property]
        => (decoder: property is Decodable in Text)
        =>  SystemProperty[unknown, property] =

    decoder.decoded(_)
