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
import anticipation.*
import turbulence.*
import perforate.*

import sun.misc as sm

trait Executive[ReturnType, CliType <: Cli]:
  def cli
      (fullArguments: Iterable[Text], environment: Environment, workingDirectory: WorkingDirectory,
          context: ProcessContext)
      : CliType
  
  def process(cli: CliType, result: ReturnType): ExitStatus 

package executives:
  given direct: Executive[ExitStatus, CliInvocation] with
    
    def cli
        (arguments: Iterable[Text], environment: Environment, workingDirectory: WorkingDirectory,
            context: ProcessContext): CliInvocation =
      
      CliInvocation(Cli.arguments(arguments), environments.jvm, workingDirectories.default, context)

    def process(cli: CliInvocation, exitStatus: ExitStatus): ExitStatus = exitStatus

def application
    [ReturnType, CliType <: Cli]
    (using executive: Executive[ReturnType, CliType])
    (arguments: Iterable[Text])
    (block: Cli ?=> ReturnType)
    : Unit =

  block(using CliInvocation(Cli.arguments(arguments), environments.jvm, workingDirectories.default, ProcessContext(stdioSources.jvm)))

case class CliInvocation
    (arguments: List[Argument], environment: Environment, workingDirectory: WorkingDirectory,
        context: ProcessContext)
extends Cli, Stdio:
  export context.stdio.{out, err, in}
  
  type State = Unit
  def initialState: Unit = ()

  def listenForSignals(signals: Signal*): LazyList[Signal] = 
    val funnel: Funnel[Signal] = Funnel()
    
    signals.foreach: signal =>
      sm.Signal.handle(sm.Signal(signal.shortName.s), event => funnel.put(signal))
    
    funnel.stream