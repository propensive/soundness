                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                    ╭───╮                                         ┃
┃  ╭─────────╮                                       │   │                                         ┃
┃  │   ╭─────╯╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮  ┃
┃  │   ╰─────╮│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯  ┃
┃  ╰─────╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ├╌╯╌─╯╰─╌ ╰───╮╰─╌ ╰───╮  ┃
┃  ╭─────╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╌   │╭───╌   │  ┃
┃  ╰─────────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯  ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0. © Copyright 2023-25 Jon Pretty, Propensive OÜ.                     ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        http://www.apache.org/licenses/LICENSE-2.0                                                ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package ambience

import language.experimental.captureChecking
import language.dynamics

import scala.compiletime.ops.string.*

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import proscenium.*
import vacuous.*

trait SystemProperty[NameType <: String, PropertyType]:
  def read(value: Text): PropertyType

object SystemProperty:
  given generic: [UnknownType <: String & Singleton] => (erased Void)
  =>    SystemProperty[UnknownType, Text] =
    identity(_)

  given javaHome: [PathType: Instantiable across Paths from Text]
  =>    SystemProperty["java.home", PathType] =
    PathType(_)

  given javaLibraryPath[PathType: Instantiable across Paths from Text]
     (using systemProperties: SystemProperties, systemProperty: Tactic[SystemPropertyError])
  :     SystemProperty["java.library.path", List[PathType]] =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(PathType(_))

  given javaClassPath[PathType: Instantiable across Paths from Text]
     (using systemProperties: SystemProperties, systemProperty: Tactic[SystemPropertyError])
  :     SystemProperty["java.class.path", List[PathType]] =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(PathType(_))

  given javaVersion: SystemProperty["java.version", Text] = identity(_)
  given javaRuntimeVersion: SystemProperty["java.runtime.version", Text] = identity(_)

  given javaExtDirs[PathType: Instantiable across Paths from Text]
     (using systemProperties: SystemProperties, systemProperty: Tactic[SystemPropertyError])
  :     SystemProperty["java.ext.dirs", List[PathType]] =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(PathType(_))

  given fileSeparator: SystemProperty["file.separator", Char] = _.decode[Char]
  given pathSeparator: SystemProperty["path.separator", Char] = _.decode[Char]
  given lineSeparator: SystemProperty["line.separator", Text] = identity(_)

  given userName: SystemProperty["user.name", Text] = identity(_)

  given userHome: [PathType: Instantiable across Paths from Text]
  =>    SystemProperty["user.home", PathType] =
    PathType(_)

  given userDir: [PathType: Instantiable across Paths from Text]
  =>    SystemProperty["user.dir", PathType] =
    PathType(_)

  given osName: SystemProperty["os.name", Text] = identity(_)
  given osVersion: SystemProperty["os.version", Text] = identity(_)
  given osArch: SystemProperty["os.arch", Text] = identity(_)

  given decoder: [UnknownType <: String & Singleton, PropertyType]
  =>   (decoder: PropertyType is Decodable in Text)
  =>    SystemProperty[UnknownType, PropertyType] =

    decoder.decoded(_)
