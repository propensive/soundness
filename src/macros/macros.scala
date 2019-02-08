/*
  
  Guillotine, version 0.2.0. Copyright 2018 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use
  this file except in compliance with the License. You may obtain a copy of the
  License at
  
      http://www.apache.org/licenses/LICENSE-2.0
 
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
  License for the specific language governing permissions and limitations under
  the License.

*/
package guillotine

import contextual._
import concurrent._, ExecutionContext.Implicits.global
import language.higherKinds
import java.io.File
import scala.io.Source
import mitigation._

import scala.collection.JavaConverters._
import scala.util._

object environments {
  implicit val enclosing: Environment = {
    Environment(mapAsScalaMapConverter(System.getenv).asScala.toMap, None)
  }

  implicit val none: Environment = Environment(Map(), None)
}

case class Environment(variables: Map[String, String], workDir: Option[String]) {
  private[guillotine] lazy val envArray: Array[String] =
    variables.map { case (k, v) => s"$k=$v" }.to[Array]

  private[guillotine] lazy val toJavaMap: java.util.Map[String, String] =
    variables.foldLeft(new java.util.HashMap[String, String]()) { case (m, (k, v)) =>
      m.put(k, v)
      m
    }

  private[guillotine] lazy val workDirFile: File =
    new File(workDir.getOrElse(System.getenv("PWD")))

  def append(key: String, value: String) = copy(variables = variables.updated(key, value))
}

case class ShellFailure(command: String, stdout: String, stderr: String) extends Exception {
  override def toString(): String = s"exec failed: $stderr"
}

object Executor {

  /*implicit val running: Executor[Running] = { (args, env) =>
    val runtime = Runtime.getRuntime
    val argsArray = args.to[Array]
    val proc = runtime.exec(argsArray, env.envArray, env.workDirFile)

    val out = scala.io.Source.fromInputStream(proc.getInputStream)
    val err = scala.io.Source.fromInputStream(proc.getErrorStream)
    Running(proc, out, err)
  }*/

  implicit val result: Executor[Result[String, ~ | ShellFailure]] = {
    (args, env) =>
      val exit = string.exec(args, env)
      if(exit.status == 0) Result.answer(exit.result)
      else {
        val cmd = args.map { a => if(a.contains(' ')) s"'$a'" else a }.mkString(" ")
        Result.abort(ShellFailure(cmd, exit.result, exit.errorStream.getLines.mkString("\n")))
      }
  }

  implicit val scalaUtilTry: Executor[Try[String]] = { (args, env) =>
    val cmd = args.map { a => if(a.contains(' ')) s"'$a'" else a }.mkString(" ")
    try {
      val exit = string.exec(args, env)
      if(exit.status == 0) Success(exit.result)
      else {
        Failure(ShellFailure(cmd, exit.result, exit.errorStream.getLines.mkString("\n")))
      }
    } catch {
      case e: java.io.IOException => {
        Failure(ShellFailure(cmd, "", e.getMessage))
      }
    }
  }

  implicit val string: Executor[Exit[String]] = { (args, env) =>
    val runtime = Runtime.getRuntime
    val argsArray = args.to[Array]
    val proc = runtime.exec(argsArray, env.envArray, env.workDirFile)

    Exit(
      proc.waitFor,
      scala.io.Source.fromInputStream(proc.getInputStream).getLines.mkString("\n").trim,
      scala.io.Source.fromInputStream(proc.getErrorStream)
    )
  }

  implicit val int: Executor[Exit[Int]] = string.exec(_, _).map(_.toInt)

  implicit def unwrapped[T](implicit executor: Executor[Exit[T]]): Executor[T] =
    executor.exec(_, _).result

  implicit def running[T: Executor]: Executor[Run[T]] =
    (args, env) => Run[T](Future(implicitly[Executor[T]].exec(args, env)))
}

trait Executor[T] {
  def exec(args: Seq[String], env: Environment): T
  def map[S](fn: T => S): Executor[S] = (args, env) => fn(exec(args, env))
}

case class Run[T](result: Future[T])

case class Exit[T](status: Int, result: T, errorStream: Source) {
  def map[S](fn: T => S): Exit[S] = Exit[S](status, fn(result), errorStream)
}


case class RunningData(proc: Process, outPump: Thread, errPump: Thread)

case class Running(runningDataOption: Option[RunningData]) {
   def await(): Int = {
    runningDataOption match {
      case Some(RunningData(proc,outPump,errPump)) =>  {
        outPump.start()
        errPump.start()
        val result = proc.waitFor()
        outPump.join()
        errPump.join()
        result
      }
      case None => 127
    }
   }

   def destroy(): Int = {
    runningDataOption match {
      case Some(RunningData(proc,_,_)) =>  {
        proc.destroy()
        await()
      }
      case None => 127
    }
   }
 }

object `package` {

  case class Command(args: String*) {
    override def toString = args.map { a => if(a.contains(" ")) '"'+a+'"' else a }.mkString(" ")

    def exec[T: Executor]()(implicit env: Environment): T = implicitly[Executor[T]].exec(args, env)

    def async[T](stdout: String => Unit = { (_: String) => () }, stderr: String => Unit = { (_: String) => () })(implicit env: Environment): Running = {
      val pb = new ProcessBuilder(args: _*)
      val envMap = pb.environment
      env.variables.foreach { case (k, v) => envMap.put(k, v) }
      env.workDir.foreach { wd => pb.directory(new java.io.File(wd)) }

      try {
        val proc = pb.start()
        val out = new java.io.BufferedReader(new java.io.InputStreamReader(proc.getInputStream()))
        val err = new java.io.BufferedReader(new java.io.InputStreamReader(proc.getErrorStream()))

        class Pump(stream: java.io.BufferedReader, send: String => Unit) extends Thread {
          override def run() = {
            var line = stream.readLine()
            while(line != null) {
              send(line)
              line = stream.readLine()
            }
          }

        }
        Running(Some(RunningData(proc, new Pump(out, stdout), new Pump(err, stderr))))
      } catch {
        case e: java.io.IOException => {
          stderr(e.getMessage)
            Running(None)
        }
      }
    }
  }

  sealed trait ShellContext extends Context
  case object SingleQuoted extends ShellContext
  case object DoubleQuoted extends ShellContext
  case object Unquoted extends ShellContext
  case object Awaiting extends ShellContext

  object ShellInterpolator extends Interpolator {
    type ContextType = ShellContext
    type Input = String
    type Output = Command

    def evaluate(interpolation: RuntimeInterpolation): Command = {
      val command = interpolation.parts.mkString
      val (_, params, _) = parseLiteral(Awaiting, command)
      Command(params: _*)
    }

    def contextualize(interpolation: StaticInterpolation): Seq[ContextType] = {
      val (contexts, finalState) =
        interpolation.parts.foldLeft((List[ContextType](), Awaiting: ShellContext)) {
          case ((contexts, state), lit @ Literal(_, string)) =>
            val (newState, _, _) = parseLiteral(state, string)
            (contexts, newState)

          case ((contexts, state), hole @ Hole(_, _)) =>
            val newState = hole(state).getOrElse(
              interpolation.abort(hole, "this type cannot be substituted here")
            )
            (newState :: contexts, newState)
        }

      if (finalState == SingleQuoted || finalState == DoubleQuoted) {
        val lit @ Literal(_, _) = interpolation.parts.last
        interpolation.abort(lit, lit.string.length, "unclosed quoted parameter")
      }

      contexts
    }

    private def parseLiteral(state: ContextType,
                             string: String): (ContextType, List[String], Boolean) =
      string.foldLeft((state, List[String](), false)) {
        case ((Awaiting, params, escape), ' ') =>
          (Awaiting, params, false)

        case ((state @ (Unquoted | DoubleQuoted | Awaiting), params, false), '\\') =>
          (state, params, true)

        case ((Unquoted, params, escape), ' ') =>
          (Awaiting, params, false)

        case ((SingleQuoted, params, escape), '\'') =>
          (Unquoted, params, false)

        case ((DoubleQuoted, params, false), '"') =>
          (Unquoted, params, false)

        case ((Unquoted, params, false), '"') =>
          (DoubleQuoted, params, false)

        case ((Unquoted, params, false), '\'') =>
          (SingleQuoted, params, false)

        case ((Awaiting, params, false), '"') =>
          (DoubleQuoted, params :+ "", false)

        case ((Awaiting, params, false), '\'') =>
          (SingleQuoted, params :+ "", false)

        case ((Awaiting, params, escape), ch) =>
          (Unquoted, params :+ s"$ch", false)

        case ((state, Nil, escape), ch) =>
          (state, List(s"$ch"), false)

        case ((state, rest :+ cur, escape), ch) =>
          (state, rest :+ s"$cur$ch", false)
      }
  }

  def esc1(s: String): String = s.replaceAll("\\\"", "\\\\\"")
  def esc2(s: String): String = s.replaceAll("'", """'"'"'""")

  implicit def embedSeqs[Coll[T] <: Seq[T]] = ShellInterpolator.embed[Coll[String]](
    Case(Awaiting, Unquoted)(_.map('"' + esc1(_) + '"').mkString(" ")),
    Case(Unquoted, Unquoted)(_.map('"' + esc1(_) + '"').mkString(" ")),
    Case(SingleQuoted, SingleQuoted)(_.map(esc2(_)).mkString(" ")),
    Case(DoubleQuoted, DoubleQuoted)(_.map(esc1(_)).mkString(" "))
  )

  implicit def embedCommands[Coll[T] <: Seq[T]] = ShellInterpolator.embed[Command](
    Case(Awaiting, Unquoted)(_.args.map('"' + esc1(_) + '"').mkString(" ")),
    Case(Unquoted, Unquoted)(_.args.map('"' + esc1(_) + '"').mkString(" ")),
    Case(SingleQuoted, SingleQuoted)(_.args.map(esc2(_)).mkString(" ")),
    Case(DoubleQuoted, DoubleQuoted)(_.args.map(esc1(_)).mkString(" "))
  )

  implicit val embedStrings = ShellInterpolator.embed[String](
    Case(Awaiting, Unquoted)('"' + esc1(_) + '"'),
    Case(Unquoted, Unquoted)('"' + esc1(_) + '"'),
    Case(SingleQuoted, SingleQuoted)(esc2(_)),
    Case(DoubleQuoted, DoubleQuoted)(esc1(_))
  )

  implicit class ShellStringContext(sc: StringContext) {
    val sh = Prefix(ShellInterpolator, sc)
  }

}
