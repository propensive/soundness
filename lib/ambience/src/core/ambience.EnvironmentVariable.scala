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

import language.dynamics

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import proscenium.*
import vacuous.*

trait EnvironmentVariable[alias <: Label, +variable] extends Pure:
  inline def defaultName: Text = name.or(valueOf[alias].tt.uncamel.snake.upper)
  def name: Optional[Text] = Unset
  def read(value: Text): variable

object EnvironmentVariable extends EnvironmentVariable2:
  given path: [path: Instantiable across Paths from Text] => (system: System)
        =>  EnvironmentVariable["path", List[path]] =

    _.cut(system(t"path.separator").or(t":")).to(List).map(path(_))

  given xdgDataDirs: [path: Instantiable across Paths from Text] => (system: System)
        =>  EnvironmentVariable["xdgDataDirs", List[path]] =

    _.cut(system(t"path.separator").or(t":")).to(List).map(path(_))

  given xdgConfigDirs: [path: Instantiable across Paths from Text] => (system: System)
        =>  EnvironmentVariable["xdgConfigDirs", List[path]] =

    _.cut(system(t"path.separator").or(t":")).to(List).map(path(_))

  given xdgDataHome: [path: Instantiable across Paths from Text]
        =>  EnvironmentVariable["xdgDataHome", path] =

    path(_)

  given xdgConfigHome: [path: Instantiable across Paths from Text]
        =>  EnvironmentVariable["xdgConfigHome", path] =
    path(_)

  given xdgStateHome: [path: Instantiable across Paths from Text]
        => (EnvironmentVariable["xdgStateHome", path]) =
    path(_)

  given xdgCacheHome: [path: Instantiable across Paths from Text]
        =>  EnvironmentVariable["xdgCacheHome", path] =

    path(_)

  given xdgRuntimeDir: [path: Instantiable across Paths from Text]
        =>  EnvironmentVariable["xdgRuntimeDir", path] =
    path(_)

  given home: [path: Instantiable across Paths from Text]
        =>  EnvironmentVariable["home", path] =
    path(_)

  given mail: [path: Instantiable across Paths from Text]
        =>  EnvironmentVariable["mail", path] =
    path(_)

  given shell: [path: Instantiable across Paths from Text]
        =>  EnvironmentVariable["shell", path] =
    path(_)

  given oldpwd: [path: Instantiable across Paths from Text]
        =>  EnvironmentVariable["oldpwd", path] =
    path(_)

  given windowid: [path: Instantiable across Paths from Text]
        =>  EnvironmentVariable["windowid", path] =
    path(_)

  given editor: [path: Instantiable across Paths from Text]
        =>  EnvironmentVariable["editor", path] =
    path(_)

  given pager: [path: Instantiable across Paths from Text]
        =>  EnvironmentVariable["pager", path] =
    path(_)

  given sshAuthSock: [path: Instantiable across Paths from Text]
        =>  EnvironmentVariable["sshAuthSock", path] =
    path(_)

  given manpager: [path: Instantiable across Paths from Text]
        =>  EnvironmentVariable["manpager", path] =
    path(_)

  given columns: (Int is Decodable in Text) => EnvironmentVariable["columns", Int] = _.decode[Int]
  given lang: EnvironmentVariable["lang", Text] = identity(_)
  given display: EnvironmentVariable["display", Text] = identity(_)
  given term: EnvironmentVariable["term", Text] = identity(_)
