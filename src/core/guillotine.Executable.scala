/*
    Guillotine, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package guillotine

import language.experimental.pureFunctions

import java.io as ji

import scala.annotation.targetName
import scala.compiletime.*

import anticipation.*
import contextual.*
import contingency.*
import fulminate.*
import gossamer.*
import kaleidoscope.*
import rudiments.*
import spectacular.*

sealed trait Executable:
  type Exec <: Label

  def fork[ResultType]()(using working: WorkingDirectory)
          : Process[Exec, ResultType] logs ExecEvent raises ExecError

  def exec[ResultType: Computable]()
     (using working: WorkingDirectory)
          : ResultType logs ExecEvent raises ExecError =

    fork[ResultType]().await()

  def apply()
     (using erased intelligible: Exec is Intelligible,
                   working:      WorkingDirectory,
                   computable:   intelligible.Result is Computable)
          : intelligible.Result logs ExecEvent raises ExecError =

    fork[intelligible.Result]().await()

  def apply(command: Executable): Pipeline = command match
    case Pipeline(commands*) => this match
      case Pipeline(commands2*) => Pipeline((commands ++ commands2)*)
      case command: Command => Pipeline((commands :+ command)*)

    case command: Command    => this match
      case Pipeline(commands2*) => Pipeline((command +: commands2)*)
      case command2: Command    => Pipeline(command, command2)

  @targetName("pipeTo")
  infix def | (command: Executable): Pipeline = command(this)

object Command:
  given Command is Communicable =
    command => Message(formattedArguments(command.arguments))

  private def formattedArguments(arguments: Seq[Text]): Text =
    arguments.map: argument =>
      if argument.contains(t"\"") && !argument.contains(t"'") then t"""'$argument'"""
      else if argument.contains(t"'") && !argument.contains(t"\"") then t""""$argument""""
      else if argument.contains(t"'") && argument.contains(t"\"")
      then t""""${argument.sub(r"""\"""", t"\\\\\"")}""""
      else if argument.contains(t" ") || argument.contains(t"\t") || argument.contains(t"\\")
      then t"'$argument'"
      else argument

    . join(t" ")

  given Command is Inspectable = command =>
    val commandText: Text = formattedArguments(command.arguments)
    if commandText.contains(t"\"") then t"sh\"\"\"$commandText\"\"\"" else t"sh\"$commandText\""

  given Command is Showable = command => formattedArguments(command.arguments)

case class Command(arguments: Text*) extends Executable:
  def fork[ResultType]()(using working: WorkingDirectory)
      : Process[Exec, ResultType] logs ExecEvent raises ExecError =

    val processBuilder = ProcessBuilder(arguments.ss*)
    processBuilder.directory(ji.File(working.directory().s))

    Log.info(ExecEvent.ProcessStart(this))

    try new Process(processBuilder.start().nn)
    catch case errror: ji.IOException => abort(ExecError(this, LazyList(), LazyList()))

object Pipeline:
  given Pipeline is Communicable =
    pipeline => m"${pipeline.commands.map(_.show).join(t" | ")}"

  given Pipeline is Inspectable = _.commands.map(_.inspect).join(t" | ")
  given Pipeline is Showable = _.commands.map(_.show).join(t" | ")

case class Pipeline(commands: Command*) extends Executable:
  def fork[ResultType]()(using working: WorkingDirectory)
      : Process[Exec, ResultType] logs ExecEvent raises ExecError =

    val processBuilders = commands.map: command =>
      val processBuilder = ProcessBuilder(command.arguments.ss*)

      processBuilder.directory(ji.File(working.directory().s))

      processBuilder.nn

    Log.info(ExecEvent.PipelineStart(commands))

    val pipeline = ProcessBuilder.startPipeline(processBuilders.asJava).nn.asScala.to(List).last
    new Process[Exec, ResultType](pipeline)
