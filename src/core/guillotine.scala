/*
    Guillotine, version 0.9.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

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
import gossamer.*

import scala.collection.convert.ImplicitConversionsToJava.*
import scala.collection.convert.ImplicitConversionsToScala.*
import scala.jdk.StreamConverters.StreamHasToScala

import annotation.targetName

import java.util.HashMap as JMap
import java.io as ji

type Stream = LazyList[String]

object envs:
  val enclosing: Env = Env(System.getenv.nn.to(Map))
  val empty: Env = Env(Map())

enum Context:
  case Awaiting, Unquoted, Quotes2, Quotes1

case class State(current: Context, esc: Boolean, args: List[String])

object Executor:
  given stream: Executor[Stream] =
    proc => ji.BufferedReader(ji.InputStreamReader(proc.getInputStream)).lines().nn.toScala(LazyList)
  
  given string: Executor[String] =
    stream.map { stream => if stream.isEmpty then "" else stream.reduce(_ + "\n" + _).trim.nn }

  // given Executor[LazyList[IArray[Byte]]] =
  //   proc => ji.BufferedInputStream(proc.getInputStream)

trait Executor[T]:
  def interpret(process: java.lang.Process): T
  def map[S](fn: T => S): Executor[S] = process => fn(interpret(process))

case class Pid(value: Long)

class Process[T](process: java.lang.Process, executor: Executor[T]):
  def pid: Pid = Pid(process.pid)
  
  def stdout(limit: Int = 1024*1024*10): DataStream = Util.read(process.getInputStream.nn, limit)
  def stderr(limit: Int = 1024*1024*10): DataStream = Util.read(process.getErrorStream.nn, limit)
  def stdin(in: LazyList[IArray[Byte]]): Unit = Util.write(in, process.getOutputStream.nn)
  def await(): T = executor.interpret(process)
  def abort(force: Boolean = false): Unit =
    if force then process.destroyForcibly() else process.destroy()

sealed trait Executable:
  def fork[T]()(using env: Env, exec: Executor[T] = Executor.string): Process[T]
  
  def exec[T]()(using env: Env, exec: Executor[T] = Executor.string): T = fork[T]().await()
  
  def apply(cmd: Executable): Pipeline = cmd match
    case Pipeline(cmds*) => this match
      case Pipeline(cmds2*) => Pipeline((cmds ++ cmds2)*)
      case cmd: Command     => Pipeline((cmds :+ cmd)*)
    case cmd: Command    => this match
      case Pipeline(cmds2*) => Pipeline((cmd +: cmds2)*)
      case cmd2: Command    => Pipeline(cmd, cmd2)
  
  @targetName("pipeTo")
  infix def |(cmd: Executable): Pipeline = cmd(this)

case class Command(args: String*) extends Executable:

  def fork[T]()(using env: Env, exec: Executor[T] = Executor.string): Process[T] =
    val processBuilder = ProcessBuilder(args*)
    processBuilder.directory(env.workDirFile)
    new Process[T](processBuilder.start().nn, summon[Executor[T]])

case class Pipeline(cmds: Command*) extends Executable:
  def fork[T]()(using env: Env, exec: Executor[T] = Executor.string): Process[T] =
    new Process[T](ProcessBuilder.startPipeline(cmds.map { cmd =>
      val pb = ProcessBuilder(cmd.args*)
      pb.directory(env.workDirFile)
      pb.nn
    }).nn.to(List).last, exec)
  
case class Env(vars: Map[String, String], workDir: Maybe[String] = Unset):
  private[guillotine] lazy val envArray: Array[String] = vars.map { (k, v) => s"$k=$v" }.to(Array)
  
  private[guillotine] lazy val workDirFile: ji.File =
    ji.File(workDir.otherwise(System.getenv("PWD")))
  
case class ExecError(command: Command, stdout: Stream, stderr: Stream) extends Exception

object Interpolation:

  case class Params(params: String*)

  object Sh extends Interpolator[Params, State, Command]:
    import Context.*
  
    def complete(state: State): Command =
      val args = state.current match
        case Quotes2        => throw InterpolationError("the double quotes have not been closed")
        case Quotes1        => throw InterpolationError("the single quotes have not been closed")
        case _ if state.esc => throw InterpolationError("cannot terminate with an escape character")
        case _              => state.args
      
      Command(args*)

    def initial: State = State(Awaiting, false, Nil)

    def skip(state: State): State = insert(state, Params("x"))

    def insert(state: State, value: Params): State =
      value.params.to(List) match
        case h :: t =>
          if state.esc
          then throw InterpolationError("escaping with '\\' is not allowed immediately before a substitution")
          
          state match
            case State(Awaiting, false, args) =>
              State(Unquoted, false, args ++ (h :: t))

            case State(Unquoted, false, args :+ last) =>
              State(Unquoted, false, args ++ (s"$last$h" :: t))
            
            case State(Quotes1, false, args :+ last) =>
              State(Quotes1, false, args :+ (s"$last$h" :: t).join(" "))
            
            case State(Quotes2, false, args :+ last) =>
              State(Quotes2, false, args :+ (s"$last$h" :: t).join(" "))
            
            case _ =>
              throw Impossible("impossible parser state")
        case _ =>
          state

          
    def parse(state: State, next: String): State = next.foldLeft(state) {
      case (State(Awaiting, esc, args), ' ')          => State(Awaiting, false, args)
      case (State(Quotes1, false, rest :+ cur), '\\') => State(Quotes1, false, rest :+ s"$cur\\")
      case (State(ctx, false, args), '\\')            => State(ctx, true, args)
      case (State(Unquoted, esc, args), ' ')          => State(Awaiting, false, args)
      case (State(Quotes1, esc, args), '\'')          => State(Unquoted, false, args)
      case (State(Quotes2, false, args), '"')         => State(Unquoted, false, args)
      case (State(Unquoted, false, args), '"')        => State(Quotes2, false, args)
      case (State(Unquoted, false, args), '\'')       => State(Quotes1, false, args)
      case (State(Awaiting, false, args), '"')        => State(Quotes2, false, args :+ "")
      case (State(Awaiting, false, args), '\'')       => State(Quotes1, false, args :+ "")
      case (State(Awaiting, esc, args), char)         => State(Unquoted, false, args :+ s"$char")
      case (State(ctx, esc, Nil), char)               => State(ctx, false, List(s"$char"))
      case (State(ctx, esc, rest :+ cur), char)       => State(ctx, false, rest :+ s"$cur$char")
      case _                                          => throw Impossible("impossible parser state")
    }

  given Insertion[Params, String] = value => Params(value)
  given Insertion[Params, List[String]] = xs => Params(xs*)
  given Insertion[Params, Command] = cmd => Params(cmd.args*)
  given [T: Show]: Insertion[Params, T] = value => Params(summon[Show[T]].show(value).s)