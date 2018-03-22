/* Guillotine, version 0.1.0. Copyright 2018 Jon Pretty, Propensive Ltd.
 *
 * The primary distribution site is: http://co.ntextu.al/
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 */
package guillotine

import contextual._

import scala.concurrent._, ExecutionContext.Implicits.global

object Executor {

  implicit val string: Executor[Exit[String]] = { args =>
    val proc = Runtime.getRuntime.exec(args.to[Array])
    Exit(proc.waitFor, scala.io.Source.fromInputStream(proc.getInputStream).getLines.mkString("\n").trim)
  }

  implicit val int: Executor[Exit[Int]] = string.exec(_).map(_.toInt)

  implicit def unwrapped[T](implicit executor: Executor[Exit[T]]): Executor[T] =
    executor.exec(_).result


  implicit def running[T: Executor]: Executor[Run[T]] = args =>
    Run[T](Future(implicitly[Executor[T]].exec(args)))
}

trait Executor[T] {
  def exec(args: Seq[String]): T
}

case class Run[T](result: Future[T])

case class Exit[T](status: Int, result: T) {
  def map[S](fn: T => S): Exit[S] = Exit[S](status, fn(result))
}

object `package` {

  case class Command(args: String*) {
    override def toString = args.filter(!_.isEmpty).mkString("Command(", ", ", ")")

    def exec[T: Executor](): T =
      implicitly[Executor[T]].exec(args)
  }

  sealed trait ShellContext extends Context
  case object InSingleQuotes extends ShellContext
  case object InDoubleQuotes extends ShellContext
  case object InUnquotedParam extends ShellContext
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

      if(finalState == InSingleQuotes || finalState == InDoubleQuotes) {
        val lit@Literal(_, _) = interpolation.parts.last
        interpolation.abort(lit, lit.string.length, "unclosed quoted parameter")
      }

      contexts
    }

    private def parseLiteral(state: ContextType, string: String): (ContextType, List[String], Boolean) =
      string.foldLeft((state, List[String](), false)) {
        case ((Awaiting, params, escape), ' ') =>
          (Awaiting, params, false)
       
        case ((state@(InUnquotedParam | InDoubleQuotes | Awaiting), params, false), '\\') =>
          (state, params, true)

        case ((InUnquotedParam, params, escape), ' ') =>
          (Awaiting, params, false)
        
        case ((InSingleQuotes, params, escape), '\'') =>
          (InUnquotedParam, params, false)
        
        case ((InDoubleQuotes, params, false), '"') =>
          (InUnquotedParam, params, false)
        
        case ((InUnquotedParam, params, false), '"') =>
          (InDoubleQuotes, params, false)
        
        case ((InUnquotedParam, params, false), '\'') =>
          (InSingleQuotes, params, false)
        
        case ((Awaiting, params, false), '"') =>
          (InDoubleQuotes, params :+ "", false)
        
        case ((Awaiting, params, false), '\'') =>
          (InSingleQuotes, params :+ "", false)
        
        case ((Awaiting, params, escape), ch) =>
          (InUnquotedParam, params :+ s"$ch", false)
        
        case ((state, Nil, escape), ch) =>
          (state, List(s"$ch"), false)
        
        case ((state, rest :+ cur, escape), ch) =>
          (state, rest :+ s"$cur$ch", false)
      }
    }

  implicit val embedStrings = ShellInterpolator.embed[String](
    Case(Awaiting, InUnquotedParam) { s => '"'+s.replaceAll("\\\"", "\\\\\"")+'"' },
    Case(InUnquotedParam, InUnquotedParam) { s => '"'+s.replaceAll("\\\"", "\\\\\"")+'"' },
    Case(InSingleQuotes, InSingleQuotes) { s => s.replaceAll("'", """'"'"'""") },
    Case(InDoubleQuotes, InDoubleQuotes) { s => s.replaceAll("\\\"", "\\\\\"") }
  )
  
  implicit class ShellStringContext(sc: StringContext) {
    val sh = Prefix(ShellInterpolator, sc)
  }

}
