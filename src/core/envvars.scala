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
import perforate.*
import fulminate.*
import gossamer.*

import language.experimental.captureChecking
import language.dynamics

@capability
trait Environment:
  def variable(name: Text): Maybe[Text]
  def knownVariables: Set[Text] = Set()

object Environment extends Dynamic:
  given default(using Quickstart): Environment = environments.jvm

  def apply
      [VariableType]
      (variable: Text)
      (using environment: Environment, reader: EnvironmentVariable[Label, VariableType],
          environmentError: Raises[EnvironmentError])
      : VariableType^{environment, reader, environmentError} =
    environment.variable(variable).mm(reader.read).or(raise(EnvironmentError(variable))(reader.read(Text(""))))
    
  inline def selectDynamic
      [VariableType]
      (key: String)
      (using environment: Environment,
          reader: EnvironmentVariable[key.type, VariableType],
          environmentError: Raises[EnvironmentError])
      : VariableType^{environment, reader, environmentError} =
    environment.variable(reader.defaultName).mm(reader.read(_)).or:
      raise(EnvironmentError(reader.defaultName))(reader.read(Text("")))
  
@capability
trait EnvironmentVariable[AliasType <: Label, +VariableType] extends Pure:
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
      [PathType: SpecificPath]
      (using systemProperties: SystemProperties)
      : EnvironmentVariable["path", List[PathType]] =
    _.cut(systemProperties(t"path.separator").or(t":")).map(SpecificPath(_))
  
  given xdgDataDirs
      [PathType: SpecificPath]
      (using systemProperties: SystemProperties)
      : EnvironmentVariable["xdgDataDirs", List[PathType]] =
    _.cut(systemProperties(t"path.separator").or(t":")).map(SpecificPath(_))
  
  given xdgConfigDirs
      [PathType: SpecificPath]
      (using systemProperties: SystemProperties)
      : EnvironmentVariable["xdgConfigDirs", List[PathType]] =
    _.cut(systemProperties(t"path.separator").or(t":")).map(SpecificPath(_))

  given xdgDataHome[PathType: SpecificPath]: EnvironmentVariable["xdgDataHome", PathType] = SpecificPath(_)
  given xdgConfigHome[PathType: SpecificPath]: EnvironmentVariable["xdgConfigHome", PathType] = SpecificPath(_)
  given xdgStateHome[PathType: SpecificPath]: EnvironmentVariable["xdgStateHome", PathType] = SpecificPath(_)
  given xdgCacheHome[PathType: SpecificPath]: EnvironmentVariable["xdgCacheHome", PathType] = SpecificPath(_)
  given xdgRuntimeDir[PathType: SpecificPath]: EnvironmentVariable["xdgRuntimeDir", PathType] = SpecificPath(_)
  given home[PathType: SpecificPath]: EnvironmentVariable["home", PathType] = SpecificPath(_)
  given mail[PathType: SpecificPath]: EnvironmentVariable["mail", PathType] = SpecificPath(_)
  given shell[PathType: SpecificPath]: EnvironmentVariable["shell", PathType] = SpecificPath(_)
  given oldpwd[PathType: SpecificPath]: EnvironmentVariable["oldpwd", PathType] = SpecificPath(_)
  given windowid[PathType: SpecificPath]: EnvironmentVariable["windowid", PathType] = SpecificPath(_)
  given editor[PathType: SpecificPath]: EnvironmentVariable["editor", PathType] = SpecificPath(_)
  given pager[PathType: SpecificPath]: EnvironmentVariable["pager", PathType] = SpecificPath(_)
  
  given sshAgentPid(using Raises[NumberError]): EnvironmentVariable["sshAgentPid", Pid] = text =>
    Pid(text.decodeAs[Int])

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

  given jvm: Environment with
    def variable(name: Text): Maybe[Text] = Maybe(System.getenv(name.s)).mm(_.tt)
