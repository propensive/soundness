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

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import guillotine.*
import prepositional.*
import proscenium.*
import vacuous.*

trait EnvironmentVariable[AliasType <: Label, +VariableType] extends Pure:
  inline def defaultName: Text = name.or(valueOf[AliasType].tt.uncamel.snake.upper)
  def name: Optional[Text] = Unset
  def read(value: Text): VariableType

object EnvironmentVariable extends EnvironmentVariable2:
  given path: [PathType: Instantiable across Paths from Text]
  =>   (systemProperties: SystemProperties)
  =>    EnvironmentVariable["path", List[PathType]] =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(PathType(_))

  given xdgDataDirs: [PathType: Instantiable across Paths from Text]
  =>   (systemProperties: SystemProperties)
  =>    EnvironmentVariable["xdgDataDirs", List[PathType]] =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(PathType(_))

  given xdgConfigDirs: [PathType: Instantiable across Paths from Text]
  =>   (systemProperties: SystemProperties)
  =>    EnvironmentVariable["xdgConfigDirs", List[PathType]] =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(PathType(_))

  given xdgDataHome: [PathType: Instantiable across Paths from Text]
  =>    EnvironmentVariable["xdgDataHome", PathType] =

    PathType(_)

  given xdgConfigHome: [PathType: Instantiable across Paths from Text]
  =>    EnvironmentVariable["xdgConfigHome", PathType] =
    PathType(_)

  given xdgStateHome: [PathType: Instantiable across Paths from Text]
  =>    (EnvironmentVariable["xdgStateHome", PathType]) =
    PathType(_)

  given xdgCacheHome: [PathType: Instantiable across Paths from Text]
  =>    EnvironmentVariable["xdgCacheHome", PathType] =

    PathType(_)

  given xdgRuntimeDir: [PathType: Instantiable across Paths from Text]
  =>    EnvironmentVariable["xdgRuntimeDir", PathType] =
    PathType(_)

  given home: [PathType: Instantiable across Paths from Text]
  =>    EnvironmentVariable["home", PathType] =
    PathType(_)

  given mail: [PathType: Instantiable across Paths from Text]
  =>    EnvironmentVariable["mail", PathType] =
    PathType(_)

  given shell: [PathType: Instantiable across Paths from Text]
  =>    EnvironmentVariable["shell", PathType] =
    PathType(_)

  given oldpwd: [PathType: Instantiable across Paths from Text]
  =>    EnvironmentVariable["oldpwd", PathType] =
    PathType(_)

  given windowid: [PathType: Instantiable across Paths from Text]
  =>    EnvironmentVariable["windowid", PathType] =
    PathType(_)

  given editor: [PathType: Instantiable across Paths from Text]
  =>    EnvironmentVariable["editor", PathType] =
    PathType(_)

  given pager: [PathType: Instantiable across Paths from Text]
  =>    EnvironmentVariable["pager", PathType] =
    PathType(_)

  given sshAgentPid: Tactic[NumberError] => EnvironmentVariable["sshAgentPid", Pid] =
    text => Pid(text.decode[Int])

  given sshAuthSock: [PathType: Instantiable across Paths from Text]
  =>    EnvironmentVariable["sshAuthSock", PathType] =
    PathType(_)

  given manpager: [PathType: Instantiable across Paths from Text]
  =>    EnvironmentVariable["manpager", PathType] =
    PathType(_)

  given columns: (Int is Decodable in Text) => EnvironmentVariable["columns", Int] = _.decode[Int]
  given lang: EnvironmentVariable["lang", Text] = identity(_)
  given display: EnvironmentVariable["display", Text] = identity(_)
  given term: EnvironmentVariable["term", Text] = identity(_)
