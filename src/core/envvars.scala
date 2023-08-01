/*
    Ambience, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import gossamer.*

import language.experimental.captureChecking
import language.dynamics

@capability
trait Environment:
  def apply(name: Text): Maybe[Text]

object Environment extends Dynamic:
  def apply
      [VariableType]
      (variable: Text)
      (using environment: Environment, reader: EnvironmentVariable[Label, VariableType],
          environmentError: CanThrow[EnvironmentError])
      : VariableType^{environment, reader, environmentError} =
    environment(variable).mm(reader.read).or(throw EnvironmentError(variable))
    
  inline def selectDynamic
      [VariableType]
      (key: String)
      (using environment: Environment,
          reader: EnvironmentVariable[key.type, VariableType],
          environmentError: CanThrow[EnvironmentError])
      : VariableType^{environment, reader, environmentError} =
    environment(reader.defaultName).mm(reader.read(_)).or:
      throw EnvironmentError(reader.defaultName)
  
@capability
trait EnvironmentVariable[AliasType <: Label, VariableType]:
  inline def defaultName: Text = name.or(valueOf[AliasType].tt.uncamel.snake.upper)
  def name: Maybe[Text] = Unset
  def read(value: Text): VariableType

trait EnvironmentVariable2:
  given generic[UnknownType <: Label]: EnvironmentVariable[UnknownType, Text] =
    identity(_)
  
  given decoder
      [UnknownType <: Label, VariableType]
      (using decoder: Decoder[VariableType])
      : EnvironmentVariable[UnknownType, VariableType] =
    decoder.decode(_)

object EnvironmentVariable extends EnvironmentVariable2:
  given path
      [PathType: GenericPathMaker]
      (using systemProperties: SystemProperties, systemProperty: CanThrow[SystemPropertyError])
      : EnvironmentVariable["path", List[PathType]] =
    _.cut(systemProperties(t"path.separator").or(t":")).map(makeGenericPath(_))
  
  given xdgDataDirs
      [PathType: GenericPathMaker]
      (using systemProperties: SystemProperties, systemProperty: CanThrow[SystemPropertyError])
      : EnvironmentVariable["xdgDataDirs", List[PathType]] =
    _.cut(systemProperties(t"path.separator").or(t":")).map(makeGenericPath(_))
  
  given xdgConfigDirs
      [PathType: GenericPathMaker]
      (using systemProperties: SystemProperties, systemProperty: CanThrow[SystemPropertyError])
      : EnvironmentVariable["xdgConfigDirs", List[PathType]] =
    _.cut(systemProperties(t"path.separator").or(t":")).map(makeGenericPath(_))

  given xdgDataHome[PathType: GenericPathMaker]: EnvironmentVariable["xdgDataHome", PathType] =
    makeGenericPath(_)

  given xdgConfigHome[PathType: GenericPathMaker]: EnvironmentVariable["xdgConfigHome", PathType] =
    makeGenericPath(_)
  
  given xdgStateHome[PathType: GenericPathMaker]: EnvironmentVariable["xdgStateHome", PathType] =
    makeGenericPath(_)
  
  given xdgCacheHome[PathType: GenericPathMaker]: EnvironmentVariable["xdgCacheHome", PathType] =
    makeGenericPath(_)
  
  given xdgRuntimeDir[PathType: GenericPathMaker]: EnvironmentVariable["xdgRuntimeDir", PathType] =
    makeGenericPath(_)
  
  given home[PathType: GenericPathMaker]: EnvironmentVariable["home", PathType] = makeGenericPath(_)
  given mail[PathType: GenericPathMaker]: EnvironmentVariable["mail", PathType] = makeGenericPath(_)
  
  given shell[PathType: GenericPathMaker]: EnvironmentVariable["shell", PathType] =
    makeGenericPath(_)
  
  given oldPwd[PathType: GenericPathMaker]: EnvironmentVariable["oldPwd", PathType] with
    def read(variable: Text): PathType = makeGenericPath(variable)
    override def name: Text = t"OLDPWD"

  given windowId[PathType: GenericPathMaker]: EnvironmentVariable["windowId", PathType] with
    def read(variable: Text): PathType = makeGenericPath(variable)
    override def name: Text = t"WINDOWID"

  given editor[PathType: GenericPathMaker]: EnvironmentVariable["editor", PathType] =
    makeGenericPath(_)
  
  given pager[PathType: GenericPathMaker]: EnvironmentVariable["pager", PathType] =
    makeGenericPath(_)
  
  given sshAgentPid(using CanThrow[NumberError]): EnvironmentVariable["sshAgentPid", Pid] = text =>
    Pid(text.decodeAs[Int])

  given sshAuthSock[PathType: GenericPathMaker]: EnvironmentVariable["sshAuthSock", PathType] =
    makeGenericPath(_)
  
  given manpager[PathType: GenericPathMaker]: EnvironmentVariable["manpager", PathType] =
    makeGenericPath(_)
  
  given columns(using Decoder[Int]): EnvironmentVariable["columns", Int] = _.decodeAs[Int]
  given lang: EnvironmentVariable["lang", Text] = identity(_)
  
  given display: EnvironmentVariable["display", Text] = identity(_)
  given term: EnvironmentVariable["term", Text] = identity(_)

case class EnvironmentError(variable: Text)
extends Error(msg"the environment variable ${variable} was not defined")

package environments:
  given empty: Environment with
    def apply(name: Text): Unset.type = Unset

  given jvm: Environment with
    def apply(name: Text): Maybe[Text] = Option(System.getenv(name.s)).map(_.nn.tt).getOrElse(Unset)