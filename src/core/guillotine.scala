/*
    Guillotine, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import contextual.*
import rudiments.*
import vacuous.*
import contingency.*
import fulminate.*
import turbulence.*
import gossamer.*
import spectacular.*
import eucalyptus.*
import anticipation.*

import scala.jdk.StreamConverters.StreamHasToScala
import scala.quoted.*
import scala.compiletime.*

import annotation.targetName
import java.io as ji

import language.experimental.captureChecking

enum Context:
  case Awaiting, Unquoted, Quotes2, Quotes1

case class State(current: Context, escape: Boolean, arguments: List[Text])

object CommandOutput extends PosixCommandOutputs

erased trait CommandOutput[+ExecType <: Label, +ResultType]

object Executor:
  given stream: Executor[LazyList[Text]] = proc =>
    val reader = ji.BufferedReader(ji.InputStreamReader(proc.getInputStream))
    reader.lines().nn.toScala(LazyList).map(_.show)
  
  given text: Executor[Text] = proc =>
    Text.make(stream.interpret(proc).map(_.s).each(append(_)))

  given string: Executor[String] = proc =>
    Text.make(stream.interpret(proc).map(_.s).each(append(_))).s

  given dataStream(using streamCut: Raises[StreamError]): Executor[LazyList[Bytes]] =
    proc => Readable.inputStream.read(proc.getInputStream.nn)
  
  given exitStatus: Executor[ExitStatus] = _.waitFor() match
    case 0     => ExitStatus.Ok
    case other => ExitStatus.Fail(other)
  
  given unit: Executor[Unit] = exitStatus.map(_ => ())
  
  given path[PathType](using SpecificPath[PathType]): Executor[PathType] =
    proc => SpecificPath(text.interpret(proc))

@capability 
trait Executor[ResultType]:
  def interpret(process: java.lang.Process): ResultType
  
  def map[ResultType2](lambda: ResultType => ResultType2): Executor[ResultType2] =
    process => lambda(interpret(process))

trait ProcessRef:
  def pid: Pid
  def kill()(using Log[Text]): Unit
  def abort()(using Log[Text]): Unit
  def alive: Boolean
  def attend(): Unit
  def startTime[InstantType: SpecificInstant]: Optional[InstantType]
  def cpuUsage[DurationType: SpecificDuration]: Optional[DurationType]

object OsProcess:
  private def allHandles = ProcessHandle.allProcesses.nn.iterator.nn.asScala.to(List)
  
  def apply(pid: Pid)(using pidError: Raises[PidError]): OsProcess =
    val handle = ProcessHandle.of(pid.value).nn
    if handle.isPresent then new OsProcess(handle.get.nn) else abort(PidError(pid))

  def all: List[OsProcess] = allHandles.map(new OsProcess(_))
  def roots: List[OsProcess] = allHandles.filter(!_.parent.nn.isPresent).map(new OsProcess(_))
  def apply(): OsProcess = new OsProcess(ProcessHandle.current.nn)

class OsProcess private (java: ProcessHandle) extends ProcessRef:
  def pid: Pid = Pid(java.pid)
  def kill()(using Log[Text]): Unit = java.destroy()
  def abort()(using Log[Text]): Unit = java.destroyForcibly()
  def alive: Boolean = java.isAlive
  def attend(): Unit = java.onExit.nn.get()
  
  def parent: Optional[OsProcess] = 
    val parent = java.parent.nn
    if parent.isPresent then new OsProcess(parent.get.nn) else Unset
  
  def children: List[OsProcess] = java.children.nn.iterator.nn.asScala.map(new OsProcess(_)).to(List)
  
  def startTime[InstantType: SpecificInstant]: Optional[InstantType] =
    val instant = java.info.nn.startInstant.nn
    if instant.isPresent then SpecificInstant(instant.get.nn.toEpochMilli) else Unset
  
  def cpuUsage[DurationType: SpecificDuration]: Optional[DurationType] =
    val duration = java.info.nn.totalCpuDuration.nn
    if duration.isPresent then SpecificDuration(duration.get.nn.toMillis) else Unset

object Process:
  given appendable[ChunkType](using writable: Writable[ji.OutputStream, ChunkType])
          : Appendable[Process[?, ?], ChunkType]^{writable} =

    (process, stream) => process.stdin(stream)
  
  given appendableText(using streamCut: Raises[StreamError]): Appendable[Process[?, ?], Text]^{streamCut} =
    (process, stream) => process.stdin(stream.map(_.sysBytes))

class Process[+ExecType <: Label, ResultType](process: java.lang.Process) extends ProcessRef:
  def pid: Pid = Pid(process.pid)
  def alive: Boolean = process.isAlive
  def attend(): Unit = process.waitFor()
  def stdout(): LazyList[Bytes] raises StreamError = Readable.inputStream.read(process.getInputStream.nn)
  def stderr(): LazyList[Bytes] raises StreamError = Readable.inputStream.read(process.getErrorStream.nn)
  
  def stdin[ChunkType](stream: LazyList[ChunkType]^)(using writable: Writable[ji.OutputStream, ChunkType])
          : Unit^{stream, writable} =

    writable.write(process.getOutputStream.nn, stream)

  def await()(using executor: Executor[ResultType]): ResultType^{executor} = executor.interpret(process)
  
  def exitStatus(): ExitStatus = process.waitFor() match
    case 0     => ExitStatus.Ok
    case other => ExitStatus.Fail(other)
  
  def abort()(using Log[Text]): Unit =
    //Log.info(t"The process with PID ${pid.value} was aborted")
    process.destroy()
  
  def kill()(using Log[Text]): Unit =
    //Log.warn(t"The process with PID ${pid.value} was killed")
    process.destroyForcibly()

  def osProcess(using Raises[PidError]) = OsProcess(pid)
  
  def startTime[InstantType: SpecificInstant]: Optional[InstantType] =
    try
      import errorHandlers.throwUnsafely
      osProcess.startTime[InstantType]
    catch case _: PidError => Unset
  
  def cpuUsage[InstantType: SpecificDuration]: Optional[InstantType] =
    try
      import errorHandlers.throwUnsafely
      osProcess.cpuUsage[InstantType]
    catch case _: PidError => Unset

sealed trait Executable:
  type Exec <: Label

  def fork[ResultType]()(using working: WorkingDirectory, log: Log[Text], exec: Raises[ExecError])
          : Process[Exec, ResultType]^{working}
  
  def exec[ResultType]()
      (using working: WorkingDirectory, log: Log[Text], executor: Executor[ResultType], exec: Raises[ExecError])
          : ResultType^{executor, working} =
    
    fork[ResultType]().await()

  def apply[ResultType]()(using erased commandOutput: CommandOutput[Exec, ResultType])
      (using working: WorkingDirectory, log: Log[Text], executor: Executor[ResultType], exec: Raises[ExecError])
          : ResultType^{executor, working} =
    
    fork[ResultType]().await()

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
  given Communicable[Command] = command => Message(formattedArguments(command.arguments))

  private def formattedArguments(arguments: Seq[Text]): Text =
    arguments.map: argument =>
      if argument.contains(t"\"") && !argument.contains(t"'") then t"""'$argument'"""
      else if argument.contains(t"'") && !argument.contains(t"\"") then t""""$argument""""
      else if argument.contains(t"'") && argument.contains(t"\"")
        then t""""${argument.rsub(t"\\\"", t"\\\\\"")}""""
      else if argument.contains(t" ") || argument.contains(t"\t") || argument.contains(t"\\") then t"'$argument'"
      else argument
    .join(t" ")

  given Debug[Command] = command =>
    val commandText: Text = formattedArguments(command.arguments)
    if commandText.contains(t"\"") then t"sh\"\"\"$commandText\"\"\"" else t"sh\"$commandText\""
  
  given Show[Command] = command => formattedArguments(command.arguments)
  
case class Command(arguments: Text*) extends Executable:
  def fork[ResultType]()(using working: WorkingDirectory, log: Log[Text], exec: Raises[ExecError])
          : Process[Exec, ResultType] =

    val processBuilder = ProcessBuilder(arguments.ss*)
    processBuilder.directory(ji.File(working.directory().s))
    
    //Log.info(msg"Starting process ${this}")

    try new Process(processBuilder.start().nn)
    catch case errror: ji.IOException => abort(ExecError(this, LazyList(), LazyList()))

object Pipeline:
  given Communicable[Pipeline] = pipeline => msg"${pipeline.commands.map(_.show).join(t" | ")}"
  given Debug[Pipeline] = _.commands.map(_.debug).join(t" | ")
  given Show[Pipeline] = _.commands.map(_.show).join(t" | ")
  
case class Pipeline(commands: Command*) extends Executable:
  def fork[ResultType]()(using working: WorkingDirectory, log: Log[Text], exec: Raises[ExecError])
          : Process[Exec, ResultType] =

    val processBuilders = commands.map: command =>
      val processBuilder = ProcessBuilder(command.arguments.ss*)
      
      processBuilder.directory(ji.File(working.directory().s))
    
      processBuilder.nn
    
    //Log.info(msg"Starting pipelined processes ${this}")

    new Process[Exec, ResultType](ProcessBuilder.startPipeline(processBuilders.asJava).nn.asScala.to(List).last)

case class ExecError(command: Command, stdout: LazyList[Bytes], stderr: LazyList[Bytes])
extends Error(msg"execution of the command $command failed")

case class PidError(pid: Pid) extends Error(msg"the process with PID ${pid.value} is not running")

object Sh:
  case class Parameters(params: Text*)

  object Prefix extends Interpolator[Parameters, State, Command]:
    import Context.*
  
    def complete(state: State): Command =
      val arguments = state.current match
        case Quotes2           => throw InterpolationError(msg"the double quotes have not been closed")
        case Quotes1           => throw InterpolationError(msg"the single quotes have not been closed")
        case _ if state.escape => throw InterpolationError(msg"cannot terminate with an escape character")
        case _                 => state.arguments
      
      Command(arguments*)

    def initial: State = State(Awaiting, false, Nil)
    def skip(state: State): State = insert(state, Parameters(t"x"))

    def insert(state: State, value: Parameters): State =
      value.params.to(List) match
        case h :: t =>
          if state.escape then throw InterpolationError(msg"""
            escaping with '\\' is not allowed immediately before a substitution
          """)
          
          (state: @unchecked) match
            case State(Awaiting, false, arguments) =>
              State(Unquoted, false, arguments ++ (h :: t))

            case State(Unquoted, false, arguments :+ last) =>
              State(Unquoted, false, arguments ++ (t"$last$h" :: t))
            
            case State(Quotes1, false, arguments :+ last) =>
              State(Quotes1, false, arguments :+ (t"$last$h" :: t).join(t" "))
            
            case State(Quotes2, false, arguments :+ last) =>
              State(Quotes2, false, arguments :+ (t"$last$h" :: t).join(t" "))
            
        case _ =>
          state
          
    def parse(state: State, next: Text): State = next.chars.to(List).foldLeft(state): (state, next) =>
      ((state, next): @unchecked) match
        case (State(Awaiting, _, arguments), ' ')           => State(Awaiting, false, arguments)
        case (State(Quotes1, false, more :+ current), '\\') => State(Quotes1, false, more :+ t"$current\\")
        case (State(context, false, arguments), '\\')       => State(context, true, arguments)
        case (State(Unquoted, _, arguments), ' ')           => State(Awaiting, false, arguments)
        case (State(Quotes1, _, arguments), '\'')           => State(Unquoted, false, arguments)
        case (State(Quotes2, false, arguments), '"')        => State(Unquoted, false, arguments)
        case (State(Unquoted, false, arguments), '"')       => State(Quotes2, false, arguments)
        case (State(Unquoted, false, arguments), '\'')      => State(Quotes1, false, arguments)
        case (State(Awaiting, false, arguments), '"')       => State(Quotes2, false, arguments :+ t"")
        case (State(Awaiting, false, arguments), '\'')      => State(Quotes1, false, arguments :+ t"")
        case (State(Awaiting, _, arguments), char)          => State(Unquoted, false, arguments :+ t"$char")
        case (State(context, _, Nil), char)                 => State(context, false, List(t"$char"))
        case (State(context, _, more :+ current), char)     => State(context, false, more :+ t"$current$char")

  given Insertion[Parameters, Text] = value => Parameters(value)
  given Insertion[Parameters, List[Text]] = xs => Parameters(xs*)
  given Insertion[Parameters, Command] = command => Parameters(command.arguments*)
  
  given [ValueType: Parameterizable]: Insertion[Parameters, ValueType] = value =>
    Parameters(summon[Parameterizable[ValueType]].show(value))

object Parameterizable:
  given [PathType](using genericPath: GenericPath[PathType]): Parameterizable[PathType]^{genericPath} =
    _.pathText
  
  given Parameterizable[Int] = _.show
  
  given [ValueType](using encoder: Encoder[ValueType]): Parameterizable[ValueType]^{encoder} =
    new Parameterizable[ValueType]:
      def show(value: ValueType): Text = encoder.encode(value)

trait Parameterizable[-ValueType]:
  def show(value: ValueType): Text

object Guillotine:
  given Realm = realm"guillotine"
  
  def sh(context: Expr[StringContext], parts: Expr[Seq[Any]])(using Quotes): Expr[Command] =
    import quotes.reflect.*
    
    val execType = ConstantType(StringConstant(context.value.get.parts.head.split(" ").nn.head.nn))
    val bounds = TypeBounds(execType, execType)

    (Refinement(TypeRepr.of[Command], "Exec", bounds).asType: @unchecked) match
      case '[type commandType <: Command; commandType] =>
        '{${Sh.Prefix.expand(context, parts)}.asInstanceOf[commandType]}
