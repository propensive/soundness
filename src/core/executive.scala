/*
    Exoskeleton, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package exoskeleton

import profanity.*
import rudiments.*
import ambience.*
import galilei.*
import anticipation.*
import turbulence.*

import sun.misc as sm

import language.experimental.captureChecking

trait Executive:
  type Return
  type CliType <: Cli

  def cli
      (fullArguments: Iterable[Text], environment: Environment, workingDirectory: WorkingDirectory,
          stdio: Stdio, signals: LazyList[Signal])
      (using interpreter: CliInterpreter)
      : CliType
  
  def process(cli: CliType, result: Return): ExitStatus 

package executives:
  given direct: Executive with
    type Return = ExitStatus
    type CliType = CliInvocation
    
    def cli
        (arguments: Iterable[Text], environment: Environment, workingDirectory: WorkingDirectory, stdio: Stdio,
            signals: LazyList[Signal])
      (using interpreter: CliInterpreter)
        : CliInvocation =
      
      CliInvocation(Cli.arguments(arguments), environments.jvm, workingDirectories.default, stdio, signals)

    def process(cli: CliInvocation, exitStatus: ExitStatus): ExitStatus = exitStatus

def application
    (using executive: Executive)
    (arguments: Iterable[Text], signals: List[Signal] = Nil)
    (block: Cli ?=> executive.Return)
    : Unit =
  
  def listen: LazyList[Signal] = 
    val funnel: Funnel[Signal] = Funnel()
    signals.foreach { signal => sm.Signal.handle(sm.Signal(signal.shortName.s), event => funnel.put(signal)) }
    funnel.stream

  val arguments2 = Cli.arguments(arguments)
  val cli = CliInvocation(arguments2, environments.jvm, workingDirectories.default, stdioSources.jvm, listen)
  
  block(using cli)

case class CliInvocation
    (arguments: List[Argument], environment: Environment, workingDirectory: WorkingDirectory, stdio: Stdio,
        signals: LazyList[Signal])
extends Cli, Stdio:
  export stdio.{out, err, in}
  
  type State = Unit
  def initialState: Unit = ()
  def readParameter[OperandType](flag: Flag[OperandType])(using FlagInterpreter[OperandType], Suggestions[OperandType]): Maybe[OperandType] = Unset

trait ShellContext:
  def scriptName: Text
  def script: Path

@capability
erased trait Effectful
