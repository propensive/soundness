                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package guillotine

import scala.language.experimental.pureFunctions

import java.io as ji

import scala.annotation.targetName

import ambience.*
import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import kaleidoscope.*
import rudiments.*
import spectacular.*
import vacuous.*

sealed trait Executable:
  type Exec <: Label

  // Explicit `using` evidence instead of `raises`/`logs` sugar, and a fresh (`^`) result:
  // the freshly-minted `Job` capability cannot cross the nested context-function results the
  // sugar desugars to (the stacked-raises convention; see rep/DECISIONS.md).
  //
  // The process runs in the given working directory, with the given environment: exactly its
  // variables, where it can enumerate them, and otherwise the JVM's own.
  def fork[result]()(using working: WorkingDirectory, environment: Environment)
    ( using Tactic[Exec.Error], (Exec.Event is Loggable)^ )
  :   Job[Exec, result]^


  // Real `using` clauses rather than the `raises`/`logs` sugar: a context-function result
  // would hide the `computable` parameter, which the separation checker rejects.
  def exec[result]()
    ( using computable:  (result is Computable)^,
            working:     WorkingDirectory,
            environment: Environment )
    ( using Tactic[Exec.Error], (Exec.Event is Loggable)^ )
  :   result =

    fork[result]().await()


  // Inline, so the context-function sugar never becomes a checked result type (which would
  // hide the `computable` parameter); the body is checked at each expansion site instead.
  inline def apply()
    ( using erased intelligible: Exec is Intelligible,
            working:             WorkingDirectory,
            environment:         Environment,
            computable:          (intelligible.Result is Computable)^ )
  :   intelligible.Result raises Exec.Error logs Exec.Event =

    fork[intelligible.Result]().await()


  def apply(command: Executable): Pipeline = command match
    case Pipeline(commands*) =>
      this match
        case Pipeline(commands2*) => Pipeline((commands ++ commands2)*)
        case command: Command     => Pipeline((commands :+ command)*)

    case command: Command =>
      this match
        case Pipeline(commands2*) => Pipeline((command +: commands2)*)
        case command2: Command    => Pipeline(command, command2)

  @targetName("pipeTo")
  infix def | (command: Executable): Pipeline = command(this)

object Command:
  private def formattedArguments(arguments: List[Text]): Text =
    arguments.map: (argument: Text) =>
      if argument.contains(t"\"") && !argument.contains(t"'") then t"""'$argument'"""
      else if argument.contains(t"'") && !argument.contains(t"\"") then t""""$argument""""
      else if argument.contains(t"'") && argument.contains(t"\"")
      then t""""${argument.sub(r"""\"""", t"\\\\\"")}""""
      else if argument.contains(t" ") || argument.contains(t"\t") || argument.contains(t"\\")
      then t"'$argument'"
      else argument

    . join(t" ")

  // Subtype-bounded, so that the type `sh"…"` expands to — a refinement of `Command`, not
  // `Command` itself — still finds this instance rather than falling through to `showable`.
  given inspectable: [command <: Command] => command is Inspectable = command =>
    val commandText: Text = formattedArguments(command.arguments.to(List))
    if commandText.contains(t"\"") then t"sh\"\"\"$commandText\"\"\"" else t"sh\"$commandText\""

  given showable: Command is Showable = command => formattedArguments(command.arguments.to(List))

  // A process builder for the arguments, in the working directory and with the environment. An
  // environment which can enumerate its variables replaces the JVM's in the child, and since the
  // JDK finds a command named without a directory on the JVM's own `PATH`, not the child's, such a
  // command is first looked up on the environment's `PATH`; one not found there is left to the
  // JDK.
  private[guillotine] def builder(arguments: List[Text])
    ( using working: WorkingDirectory, environment: Environment )
  :   ProcessBuilder =

    // The `java.util.List` overload, not the varargs one: a Java varargs splice of an array
    // value is rejected under separation checking.
    val javaArguments = java.util.ArrayList[String]()
    arguments.map(_.s).each(javaArguments.add(_))
    val processBuilder = ProcessBuilder(javaArguments)
    processBuilder.directory(ji.File(working.directory().s))

    environment.entries.let: entries =>
      if !javaArguments.isEmpty then javaArguments.set(0, locate(javaArguments.get(0).nn.tt).s)
      val variables = processBuilder.environment().nn
      variables.clear()
      entries.stdlib.foreach { (name, value) => variables.put(name.s, value.s) }

    processBuilder

  // The executable a command name refers to on the environment's `PATH`: the name itself if it
  // contains a directory, or if no directory on the `PATH` holds an executable of that name.
  private def locate(name: Text)(using environment: Environment): Text =
    if name.contains(t"/") || name.contains(t"\\") then name else
      val extensions: List[Text] =
        if ji.File.separatorChar == '\\'
        then t"" :: environment.variable(t"PATHEXT").or(t".EXE").cut(t";")
        else List(t"")

      val candidates: List[ji.File] =
        environment.variable(t"PATH").or(t"").cut(ji.File.pathSeparator.nn.tt).bind: directory =>
          if directory == t"" then Nil
          else extensions.map { extension => ji.File(directory.s, t"$name$extension".s) }

      val found: Optional[ji.File] = candidates.seek { file => file.isFile && file.canExecute }
      found.lay(name)(_.getAbsolutePath.nn.tt)

case class Command(arguments: Text*) extends Executable:
  def fork[result]()(using working: WorkingDirectory, environment: Environment)
    ( using Tactic[Exec.Error], (Exec.Event is Loggable)^ )
  :   Job[Exec, result]^ =

    val processBuilder = Command.builder(arguments.to(List))

    Log.info(Exec.Event.ProcessStart(this))

    // The JDK process starts inside the `try`; the `Job` capability is minted outside it
    // (a fresh result may not be created within a `try` expression).
    val process =
      try processBuilder.start().nn
      catch case errror: ji.IOException => abort(Exec.Error(this))

    new Job(process)


  def escape: Text = arguments.map { argument => t"'${argument.sub(t"'", t"\'")}'" }.join(t" ")

object Pipeline:
  given communicable: Pipeline is Communicable =
    pipeline => m"${pipeline.commands.map(_.show).join(t" | ")}"

  // Subtype-bounded for the same reason as `Command.inspectable`, above.
  given inspectable: [pipeline <: Pipeline] => pipeline is Inspectable =
    _.commands.map(_.inspect).join(t" | ")
  given showable: Pipeline is Showable = _.commands.map(_.show).join(t" | ")

case class Pipeline(commands: Command*) extends Executable:
  def fork[result]()(using working: WorkingDirectory, environment: Environment)
    ( using Tactic[Exec.Error], (Exec.Event is Loggable)^ )
  :   Job[Exec, result]^ =

    val processBuilders = commands.map(_.arguments.to(List)).map(Command.builder(_))

    Log.info(Exec.Event.PipelineStart(commands.to(List)))

    val pipeline = ProcessBuilder.startPipeline(processBuilders.asJava).nn.asScala

    // The tail speaks for the pipeline — its output and its exit status are the pipeline's — but
    // input is written to the head: `startPipeline` wires every later process's standard input to
    // its predecessor's output, and replaces it with a stream that throws on any write.
    new Job[Exec, result](pipeline.last, pipeline.head)
