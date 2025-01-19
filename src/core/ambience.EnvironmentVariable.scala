/*
    Ambience, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package ambience

import language.experimental.captureChecking
import language.dynamics

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

trait EnvironmentVariable[AliasType <: Label, +VariableType] extends Pure:
  inline def defaultName: Text = name.or(valueOf[AliasType].tt.uncamel.snake.upper)
  def name: Optional[Text] = Unset
  def read(value: Text): VariableType

object EnvironmentVariable extends EnvironmentVariable2:
  given path: [PathType: SpecificPath] => (systemProperties: SystemProperties)
      => EnvironmentVariable["path", List[PathType]] =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(SpecificPath(_))

  given xdgDataDirs: [PathType: SpecificPath] => (systemProperties: SystemProperties)
      => EnvironmentVariable["xdgDataDirs", List[PathType]] =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(SpecificPath(_))

  given xdgConfigDirs: [PathType: SpecificPath] => (systemProperties: SystemProperties)
      => EnvironmentVariable["xdgConfigDirs", List[PathType]] =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(SpecificPath(_))

  given xdgDataHome: [PathType: SpecificPath as specific]
      => EnvironmentVariable["xdgDataHome", PathType] =

    SpecificPath(_)

  given xdgConfigHome: [PathType: SpecificPath as specific]
      => EnvironmentVariable["xdgConfigHome", PathType] =
    SpecificPath(_)

  given xdgStateHome: [PathType: SpecificPath] => (EnvironmentVariable["xdgStateHome", PathType]) =
    SpecificPath(_)

  given xdgCacheHome: [PathType: SpecificPath as specific]
  =>  EnvironmentVariable["xdgCacheHome", PathType] =

    SpecificPath(_)

  given xdgRuntimeDir: [PathType: SpecificPath] => EnvironmentVariable["xdgRuntimeDir", PathType] =
    SpecificPath(_)

  given home: [PathType: SpecificPath] => EnvironmentVariable["home", PathType] = SpecificPath(_)
  given mail: [PathType: SpecificPath] => EnvironmentVariable["mail", PathType] = SpecificPath(_)
  given shell: [PathType: SpecificPath] => EnvironmentVariable["shell", PathType] = SpecificPath(_)
  given oldpwd: [PathType: SpecificPath] => EnvironmentVariable["oldpwd", PathType] = SpecificPath(_)
  given windowid: [PathType: SpecificPath] => EnvironmentVariable["windowid", PathType] = SpecificPath(_)
  given editor: [PathType: SpecificPath] => EnvironmentVariable["editor", PathType] = SpecificPath(_)
  given pager: [PathType: SpecificPath] => EnvironmentVariable["pager", PathType] = SpecificPath(_)

  given sshAgentPid: Tactic[NumberError] => EnvironmentVariable["sshAgentPid", Pid] =
    text => Pid(text.decode[Int])

  given sshAuthSock[PathType: SpecificPath]: EnvironmentVariable["sshAuthSock", PathType] = SpecificPath(_)
  given manpager[PathType: SpecificPath]: EnvironmentVariable["manpager", PathType] = SpecificPath(_)
  given columns: Decoder[Int] => EnvironmentVariable["columns", Int] = _.decode[Int]
  given lang: EnvironmentVariable["lang", Text] = identity(_)
  given display: EnvironmentVariable["display", Text] = identity(_)
  given term: EnvironmentVariable["term", Text] = identity(_)
