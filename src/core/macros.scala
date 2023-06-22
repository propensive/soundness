/*
    Probably, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package probably

import gossamer.*
import rudiments.*
import chiaroscuro.*
import spectacular.*

import dotty.tools.dotc.util as dtdu
import scala.quoted.*

object Probably:
  protected def general
      [T: Type, R: Type, S: Type]
      (test: Expr[Test[T]], pred: Expr[T => Boolean], runner: Expr[Runner[R]],
          inc: Expr[Inclusion[R, Outcome]], inc2: Expr[Inclusion[R, DebugInfo]],
          action: Expr[TestRun[T] => S])
      (using Quotes)
      : Expr[S] =
    import quotes.reflect.*
    
    val exp: Option[Expr[Any]] = pred.asTerm match
      case Inlined(_, _, Block(List(DefDef(a1, _, _, Some(expression))), Closure(Ident(a2), _)))
          if a1 == a2 =>
        expression match
          case Apply(Select(Ident(_), "=="), List(term)) => Some(term.asExpr)
          case Apply(Select(term, "=="), List(Ident(_))) => Some(term.asExpr)
          case other                                     => None
      
      case _ =>
        None
    
    exp match
      case Some('{ $expr: t }) =>
        val debug: Expr[Debug[t | T]] =
          Expr.summon[Debug[t | T]].getOrElse('{ TextConversion.any })
        
        val contrast = Expr.summon[Contrast[t | T]].get
        '{ assertion[t | T, T, R, S]($runner, $test, $pred, $action, $contrast, Some($expr), $inc,
            $inc2, $debug) }
      
      case _ =>
        '{ assertion[T, T, R, S]($runner, $test, $pred, $action, Contrast.nothing[T], None, $inc,
            $inc2, TextConversion.any) }
  
  def check
      [T: Type, R: Type]
      (test: Expr[Test[T]], pred: Expr[T => Boolean], runner: Expr[Runner[R]],
          inc: Expr[Inclusion[R, Outcome]], inc2: Expr[Inclusion[R, DebugInfo]])
      (using Quotes)
      : Expr[T] =
    general[T, R, T](test, pred, runner, inc, inc2, '{ (t: TestRun[T]) => t.get })
    
  def assert
      [T: Type, R: Type]
      (test: Expr[Test[T]], pred: Expr[T => Boolean], runner: Expr[Runner[R]],
          inc: Expr[Inclusion[R, Outcome]], inc2: Expr[Inclusion[R, DebugInfo]])
      (using Quotes)
      : Expr[Unit] =
    general[T, R, Unit](test, pred, runner, inc, inc2, '{ (t: TestRun[T]) => () })
  
  def aspire
      [T: Type, R: Type]
      (test: Expr[Test[T]], runner: Expr[Runner[R]], inc: Expr[Inclusion[R, Outcome]],
          inc2: Expr[Inclusion[R, DebugInfo]])
      (using Quotes)
      : Expr[Unit] =
    general[T, R, Unit](test, '{ _ => true }, runner, inc, inc2, '{ (t: TestRun[T]) => () })

  def succeed: Any => Boolean = (value: Any) => true
  
  def assertion
      [T, T0 <: T, R, S]
      (runner: Runner[R], test: Test[T0], pred: T0 => Boolean, result: TestRun[T0] => S,
          contrast: Contrast[T], exp: Option[T], inc: Inclusion[R, Outcome],
          inc2: Inclusion[R, DebugInfo], display: Debug[T]): S =
    runner.run(test).pipe: run =>
      val outcome = run match
        case TestRun.Throws(err, duration, map) =>
          val exception: Exception =
            try err() catch case exc: Exception => exc
          if !map.isEmpty then inc2.include(runner.report, test.id, DebugInfo.Captures(map))
          Outcome.Throws(exception, duration)
    
        case TestRun.Returns(value, duration, map) =>
          try if pred(value) then Outcome.Pass(duration) else
            exp match
              case Some(exp) =>
                inc2.include(runner.report, test.id, DebugInfo.Compare(display(exp),
                    display(value), contrast(exp, value)))
              case None =>
                //inc2.include(runner.report, test.id, DebugInfo.Compare(summon[Contrast[Any]].compare(value, 1)))
            
            if !map.isEmpty then inc2.include(runner.report, test.id, DebugInfo.Captures(map))
            
            Outcome.Fail(duration)
          catch case err: Exception => Outcome.CheckThrows(err, duration)

      inc.include(runner.report, test.id, outcome)
      result(run)

  def inspect[T: Type](expr: Expr[T], test: Expr[TestContext])(using Quotes): Expr[T] =
    import quotes.reflect.*

    val exprName: Text = expr.asTerm.pos match
      case pos: dtdu.SourcePosition => pos.lineContent.show.slice(pos.startColumn, pos.endColumn)
      case _                        => t"<unknown>"
    
    val debug: Expr[Debug[T]] =
      Expr.summon[Debug[T]].getOrElse('{ TextConversion.any })
    
    '{ $test.capture(Text(${Expr[String](exprName.s)}), $expr)(using $debug) }
