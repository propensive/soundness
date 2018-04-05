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

case class WorkDir(dir: Option[String])
case class Environment(values: Map[String, String])

object Executor {

  implicit val string: Executor[Exit[String]] = { (args, env, workDir) =>
    val proc = workDir match {
      case Some(wd) => Runtime.getRuntime.exec(args.to[Array], env.map { case (k, v) => s"$k=$v" }.to[Array], new File(wd))
      case None => Runtime.getRuntime.exec(args.to[Array])
    }
    
    Exit(proc.waitFor, scala.io.Source.fromInputStream(proc.getInputStream).getLines.mkString("\n").trim)
  }

  implicit val int: Executor[Exit[Int]] = string.exec(_, _, _).map(_.toInt)

  implicit def unwrapped[T](implicit executor: Executor[Exit[T]]): Executor[T] =
    executor.exec(_, _, _).result


  implicit def running[T: Executor]: Executor[Run[T]] = (args, env, workDir) =>
    Run[T](Future(implicitly[Executor[T]].exec(args, env, workDir)))
}

trait Executor[T] {
  def exec(args: Seq[String], env: Map[String, String], workDir: Option[String]): T
}

case class Run[T](result: Future[T])

case class Exit[T](status: Int, result: T) {
  def map[S](fn: T => S): Exit[S] = Exit[S](status, fn(result))
}

object `package` {

  case class Command(args: String*) {
    override def toString = args.filter(!_.isEmpty).mkString("Command(", ", ", ")")

    def exec[T: Executor]()(implicit env: Environment, workDir: WorkDir = WorkDir(None)): T =
      implicitly[Executor[T]].exec(args, env.values, workDir.dir)
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
