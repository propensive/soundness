/*
  
  Guillotine, version 0.1.0. Copyright 2018 Jon Pretty, Propensive Ltd.

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

object environemnts {
  implicit val enclosing: Environment = {
    import scala.collection.JavaConverters._
    Environment(None, mapAsScalaMapConverter(System.getenv).asScala.toMap)
  }

  implicit val none: Environment = Environment(None, Map())
}

case class Environment(workDir: Option[String], variables: Map[String, String]) {
  private[guillotine] lazy val envArray: Array[String] =
    variables.map { case (k, v) => s"$k=$v" }.to[Array]

  private[guillotine] lazy val workDirFile: File =
    new File(workDir.getOrElse(System.getenv("PWD")))
}

object Executor {

  implicit val source: Executor[Source] = { (args, env) =>
    val runtime = Runtime.getRuntime
    val argsArray = args.to[Array]
    val proc = runtime.exec(argsArray, env.envArray, env.workDirFile)
    
    scala.io.Source.fromInputStream(proc.getInputStream)
  }
  
  implicit val string: Executor[Exit[String]] = { (args, env) =>
    val runtime = Runtime.getRuntime
    val argsArray = args.to[Array]
    val proc = runtime.exec(argsArray, env.envArray, env.workDirFile)
    
    Exit(proc.waitFor, scala.io.Source.fromInputStream(proc.getInputStream).getLines.mkString("\n").trim, scala.io.Source.fromInputStream(proc.getErrorStream))
  }

  implicit val int: Executor[Exit[Int]] = string.exec(_, _).map(_.toInt)

  implicit def unwrapped[T](implicit executor: Executor[Exit[T]]): Executor[T] =
    executor.exec(_, _).result


  implicit def running[T: Executor]: Executor[Run[T]] = (args, env) =>
    Run[T](Future(implicitly[Executor[T]].exec(args, env)))
}

trait Executor[T] {
  def exec(args: Seq[String], env: Environment): T
  def map[S](fn: T => S): Executor[S] = (args, env) => fn(exec(args, env))
}

case class Run[T](result: Future[T])

case class Exit[T](status: Int, result: T, errorStream: Source) {
  def map[S](fn: T => S): Exit[S] = Exit[S](status, fn(result), errorStream)
}

object `package` {

  case class Command(args: String*) {
    override def toString = args.filter(!_.isEmpty).mkString("Command(", ", ", ")")

    def exec[T: Executor]()(implicit env: Environment): T =
      implicitly[Executor[T]].exec(args, env)
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
      val (contexts, finalState) = interpolation.parts.foldLeft((List[ContextType](), Awaiting:
          ShellContext)) {
        case ((contexts, state), lit@Literal(_, string)) =>
          val (newState, _, _) = parseLiteral(state, string)
          (contexts, newState)

        case ((contexts, state), hole@Hole(_, _)) =>
          val newState = hole(state).getOrElse(interpolation.abort(hole,
              "this type cannot be substituted here"))
          (newState :: contexts, newState)
      }

      if(finalState == SingleQuoted || finalState == DoubleQuoted) {
        val lit@Literal(_, _) = interpolation.parts.last
        interpolation.abort(lit, lit.string.length, "unclosed quoted parameter")
      }

      contexts
    }

    private def parseLiteral(state: ContextType, string: String): (ContextType, List[String], Boolean) =
      string.foldLeft((state, List[String](), false)) {
        case ((Awaiting, params, escape), ' ') =>
          (Awaiting, params, false)
       
        case ((state@(Unquoted | DoubleQuoted | Awaiting), params, false), '\\') =>
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
    Case(Awaiting, Unquoted)(_.map('"'+esc1(_)+'"').mkString(" ")),
    Case(Unquoted, Unquoted)(_.map('"'+esc1(_)+'"').mkString(" ")),
    Case(SingleQuoted, SingleQuoted)(_.map(esc2(_)).mkString(" ")),
    Case(DoubleQuoted, DoubleQuoted)(_.map(esc1(_)).mkString(" "))
  )
  
  implicit val embedStrings = ShellInterpolator.embed[String](
    Case(Awaiting, Unquoted)('"'+esc1(_)+'"'),
    Case(Unquoted, Unquoted)('"'+esc1(_)+'"'),
    Case(SingleQuoted, SingleQuoted)(esc2(_)),
    Case(DoubleQuoted, DoubleQuoted)(esc1(_))
  )
  
  implicit class ShellStringContext(sc: StringContext) {
    val sh = Prefix(ShellInterpolator, sc)
  }

}
