/*
    Guillotine, version 0.5.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

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

import scala.collection.JavaConverters.*
import scala.jdk.StreamConverters.StreamHasToScala

import java.util.HashMap as JMap
import java.io.*

type Stream = LazyList[String]

object envs:
  val enclosing: Env = Env(System.getenv.asScala.to(Map), None)
  val empty: Env = Env(Map(), None)

enum Context:
  case Awaiting, Unquoted, Quotes2, Quotes1

case class State(current: Context, esc: Boolean, args: List[String])

object Executor:
  
  given stream: Executor[Stream] =
    proc => BufferedReader(InputStreamReader(proc.getInputStream)).lines().toScala(LazyList)
  
  given Executor[String] =
    stream.map { stream => if stream.isEmpty then "" else stream.reduce(_ + "\n" + _).trim }

trait Executor[T]:
  def interpret(process: Process): T
  def map[S](fn: T => S): Executor[S] = process => fn(interpret(process))

case class Command(args: String*):
  def exec[T: Executor]()(using Env): T =
    val processBuilder = ProcessBuilder(args*)
    processBuilder.directory(summon[Env].workDirFile)
    summon[Executor[T]].interpret(processBuilder.start())

case class Env(vars: Map[String, String], workDir: Option[String]):
  private[guillotine] lazy val envArray: Array[String] = vars.map { (k, v) => s"$k=$v" }.to(Array)
  private[guillotine] lazy val workDirFile: File = File(workDir.getOrElse(System.getenv("PWD")))
  
case class ExecError(command: Command, stdout: Stream, stderr: Stream) extends Exception

object Sh extends Interpolator[List[String], State, Command]:
  import Context.*
  
  def complete(state: State): Command =
    val args = state.current match
      case Quotes2        => throw ParseError("the double quotes have not been closed")
      case Quotes1        => throw ParseError("the single quotes have not been closed")
      case _ if state.esc => throw ParseError("an escape character is not permitted at the end")
      case _              => state.args
    
    Command(args*)

  def initial: State = State(Awaiting, false, Nil)

  def insert(state: State, value: Option[List[String]]): State = value.getOrElse(List("x")) match
    case Nil =>
      state

    case h :: t =>
      if state.esc
      then throw ParseError("escaping with '\\' is not allowed immediately before a substitution")
      
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

given Insertion[List[String], String] = value => List(value)
given Insertion[List[String], List[String]] = identity(_)
given Insertion[List[String], Command] = _.args.to(List)
