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
    environment(reader.name).mm(reader.read(_)).or:
      throw EnvironmentError(reader.name)
  
@capability
trait EnvironmentVariable[AliasType <: Label, VariableType]:
  inline def name: Text = valueOf[AliasType].tt.uncamel.snake.upper
  def read(value: Text): VariableType

object EnvironmentVariable:
  given generic[UnknownType <: Label]: EnvironmentVariable[UnknownType, Text] =
    identity(_)
  
  given decoder
      [UnknownType <: Label, VariableType]
      (using decoder: Decoder[VariableType])
      : EnvironmentVariable[UnknownType, VariableType] =
    decoder.decode(_)

case class EnvironmentError(variable: Text)
extends Error(msg"the environment variable ${variable} was not defined")

package environments:
  given empty: Environment with
    def apply(name: Text): Unset.type = Unset

  given jvm: Environment with
    def apply(name: Text): Maybe[Text] = Option(System.getenv(name.s)).map(_.nn.tt).getOrElse(Unset)