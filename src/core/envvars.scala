/*
    Ambience, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import spectacular.*
import rudiments.*
import vacuous.*
import contingency.*
import fulminate.*
import gossamer.*

import language.experimental.captureChecking
import language.dynamics

@capability
trait Environment:
  def variable(name: Text): Optional[Text]
  def knownVariables: Set[Text] = Set()

object Environment extends Dynamic:
  given default(using Quickstart): Environment = environments.virtualMachine

  def apply[VariableType](variable: Text)
      (using environment:      Environment,
             reader:           EnvironmentVariable[Label, VariableType],
             environmentError: Errant[EnvironmentError])
          : VariableType^{environment, reader, environmentError} =

    environment.variable(variable).let(reader.read).or(raise(EnvironmentError(variable))(reader.read(Text(""))))

  inline def selectDynamic[VariableType](key: String)
      (using environment:      Environment,
             reader:           EnvironmentVariable[key.type, VariableType],
             environmentError: Errant[EnvironmentError])
          : VariableType^{environment, reader, environmentError} =

    environment.variable(reader.defaultName).let(reader.read(_)).or:
      raise(EnvironmentError(reader.defaultName))(reader.read(Text("")))

@capability
trait EnvironmentVariable[AliasType <: Label, +VariableType] extends Pure:
  inline def defaultName: Text = name.or(valueOf[AliasType].tt.uncamel.snake.upper)
  def name: Optional[Text] = Unset
  def read(value: Text): VariableType

trait EnvironmentVariable2:
  given generic[UnknownType <: Label]: EnvironmentVariable[UnknownType, Text] =
    identity(_)

  given decoder[UnknownType <: Label, VariableType](using decoder: Decoder[VariableType])
          : EnvironmentVariable[UnknownType, VariableType] =

    decoder.decode(_)

object EnvironmentVariable extends EnvironmentVariable2:
  given [PathType: SpecificPath](using systemProperties: SystemProperties)
      => (EnvironmentVariable["path", List[PathType]]^{systemProperties}) as path =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(SpecificPath(_))

  given [PathType: SpecificPath](using systemProperties: SystemProperties)
      => (EnvironmentVariable["xdgDataDirs", List[PathType]]^{systemProperties}) as xdgDataDirs =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(SpecificPath(_))

  given [PathType: SpecificPath](using systemProperties: SystemProperties)
      => (EnvironmentVariable["xdgConfigDirs", List[PathType]]^{systemProperties}) as xdgConfigDirs =

    _.cut(systemProperties(t"path.separator").or(t":")).to(List).map(SpecificPath(_))

  given [PathType: SpecificPath as specific]
      => (EnvironmentVariable["xdgDataHome", PathType]^{specific}) as xdgDataHome =

    SpecificPath(_)

  given [PathType: SpecificPath as specific]
      => (EnvironmentVariable["xdgConfigHome", PathType]^{specific}) as xdgConfigHome =
    SpecificPath(_)

  given [PathType: SpecificPath]
      => (EnvironmentVariable["xdgStateHome", PathType]) as xdgStateHome =
    SpecificPath(_)

  given [PathType: SpecificPath as specific]
          => (EnvironmentVariable["xdgCacheHome", PathType]^{specific}) as xdgCacheHome =

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

  given (using Errant[NumberError]) => EnvironmentVariable["sshAgentPid", Pid] as sshAgentPid =
    text => Pid(text.decodeAs[Int])

  given sshAuthSock[PathType: SpecificPath]: EnvironmentVariable["sshAuthSock", PathType] = SpecificPath(_)
  given manpager[PathType: SpecificPath]: EnvironmentVariable["manpager", PathType] = SpecificPath(_)
  given columns(using Decoder[Int]): EnvironmentVariable["columns", Int] = _.decodeAs[Int]
  given lang: EnvironmentVariable["lang", Text] = identity(_)
  given display: EnvironmentVariable["display", Text] = identity(_)
  given term: EnvironmentVariable["term", Text] = identity(_)

case class EnvironmentError(variable: Text)
extends Error(msg"the environment variable ${variable} was not defined")

package environments:
  given empty: Environment with
    def variable(name: Text): Unset.type = Unset

  given virtualMachine: Environment with
    def variable(name: Text): Optional[Text] = Optional(System.getenv(name.s)).let(_.tt)
