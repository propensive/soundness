/*
    Ambience, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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
  given [PathType: SpecificPath](using systemProperties: SystemProperties)
      => EnvironmentVariable["path", List[PathType]] as path =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(SpecificPath(_))

  given [PathType: SpecificPath](using systemProperties: SystemProperties)
      => EnvironmentVariable["xdgDataDirs", List[PathType]] as xdgDataDirs =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(SpecificPath(_))

  given [PathType: SpecificPath](using systemProperties: SystemProperties)
      => EnvironmentVariable["xdgConfigDirs", List[PathType]] as xdgConfigDirs =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(SpecificPath(_))

  given [PathType: SpecificPath as specific]
      => EnvironmentVariable["xdgDataHome", PathType] as xdgDataHome =

    SpecificPath(_)

  given [PathType: SpecificPath as specific]
      => EnvironmentVariable["xdgConfigHome", PathType] as xdgConfigHome =
    SpecificPath(_)

  given [PathType: SpecificPath]
      => (EnvironmentVariable["xdgStateHome", PathType]) as xdgStateHome =
    SpecificPath(_)

  given [PathType: SpecificPath as specific]
          => EnvironmentVariable["xdgCacheHome", PathType] as xdgCacheHome =

    SpecificPath(_)

  given [PathType: SpecificPath]
      => EnvironmentVariable["xdgRuntimeDir", PathType] as xdgRuntimeDir =
    SpecificPath(_)

  given [PathType: SpecificPath] => EnvironmentVariable["home", PathType] as home = SpecificPath(_)
  given [PathType: SpecificPath] => EnvironmentVariable["mail", PathType] as mail = SpecificPath(_)
  given [PathType: SpecificPath] => EnvironmentVariable["shell", PathType] as shell = SpecificPath(_)
  given [PathType: SpecificPath] => EnvironmentVariable["oldpwd", PathType] as oldpwd = SpecificPath(_)
  given [PathType: SpecificPath] => EnvironmentVariable["windowid", PathType] as windowid = SpecificPath(_)
  given [PathType: SpecificPath] => EnvironmentVariable["editor", PathType] as editor = SpecificPath(_)
  given [PathType: SpecificPath] => EnvironmentVariable["pager", PathType] as pager = SpecificPath(_)

  given (using Tactic[NumberError]) => EnvironmentVariable["sshAgentPid", Pid] as sshAgentPid =
    text => Pid(text.decode[Int])

  given sshAuthSock[PathType: SpecificPath]: EnvironmentVariable["sshAuthSock", PathType] = SpecificPath(_)
  given manpager[PathType: SpecificPath]: EnvironmentVariable["manpager", PathType] = SpecificPath(_)
  given columns(using Decoder[Int]): EnvironmentVariable["columns", Int] = _.decode[Int]
  given lang: EnvironmentVariable["lang", Text] = identity(_)
  given display: EnvironmentVariable["display", Text] = identity(_)
  given term: EnvironmentVariable["term", Text] = identity(_)
