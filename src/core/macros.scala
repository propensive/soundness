/*
    Probably, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import anticipation.*
import rudiments.*
import chiaroscuro.*
import spectacular.*

import dotty.tools.dotc.util as dtdu
import scala.quoted.*

object Probably:
  protected def general[TestType: Type, ReportType: Type, ResultType: Type]
      (test:      Expr[Test[TestType]],
       predicate: Expr[TestType => Boolean],
       runner:    Expr[Runner[ReportType]],
       inc:       Expr[Inclusion[ReportType, Outcome]],
       inc2:      Expr[Inclusion[ReportType, DebugInfo]],
       action:    Expr[TestRun[TestType] => ResultType])
      (using Quotes)
          : Expr[ResultType] =

    import quotes.reflect.*
    
    val exp: Option[Expr[Any]] = predicate.asTerm match
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
        val debug: Expr[Debug[t | TestType]] =
          Expr.summon[Debug[t | TestType]].getOrElse('{ TextConversion.any })
        
        val contrast = '{Contrast.general[t | TestType]}
        //val contrast = Expr.summon[Contrast[t | TestType]].getOrElse('{Contrast.general[t | TestType]})
        '{
          assertion[t | TestType, TestType, ReportType, ResultType]
              ($runner, $test, $predicate, $action, $contrast, Some($expr), $inc, $inc2, $debug)
        }
      
      case _ =>
        '{
          assertion[TestType, TestType, ReportType, ResultType]
              ($runner, $test, $predicate, $action, Contrast.nothing[TestType], None, $inc, $inc2,
                  TextConversion.any)
        }
  
  def check[TestType: Type, ReportType: Type]
      (test:      Expr[Test[TestType]],
       predicate: Expr[TestType => Boolean],
       runner:    Expr[Runner[ReportType]],
       inc:       Expr[Inclusion[ReportType, Outcome]],
       inc2:      Expr[Inclusion[ReportType, DebugInfo]])
      (using Quotes)
          : Expr[TestType] =

    general[TestType, ReportType, TestType](test, predicate, runner, inc, inc2, '{ (t: TestRun[TestType]) => t.get })
    
  def assert[TestType: Type, ReportType: Type]
      (test:      Expr[Test[TestType]],
       predicate: Expr[TestType => Boolean],
       runner:    Expr[Runner[ReportType]],
       inc:       Expr[Inclusion[ReportType, Outcome]],
       inc2:      Expr[Inclusion[ReportType, DebugInfo]])
      (using Quotes)
          : Expr[Unit] =
    general[TestType, ReportType, Unit](test, predicate, runner, inc, inc2, '{ (t: TestRun[TestType]) => () })
  
  def aspire[TestType: Type, ReportType: Type]
      (test: Expr[Test[TestType]],
       runner: Expr[Runner[ReportType]],
       inc: Expr[Inclusion[ReportType, Outcome]],
       inc2: Expr[Inclusion[ReportType, DebugInfo]])
      (using Quotes)
          : Expr[Unit] =

    general[TestType, ReportType, Unit](test, '{ _ => true }, runner, inc, inc2, '{ (t: TestRun[TestType]) => () })

  def succeed: Any => Boolean = (value: Any) => true
  
  def assertion[TestType, TestType2 <: TestType, ReportType, ResultType]
      (runner: Runner[ReportType],
       test: Test[TestType2],
       predicate: TestType2 => Boolean,
       result: TestRun[TestType2] => ResultType,
       contrast: Contrast[TestType],
       exp: Option[TestType],
       inc: Inclusion[ReportType, Outcome],
       inc2: Inclusion[ReportType, DebugInfo],
       display: Debug[TestType])
          : ResultType =

    runner.run(test).pipe: run =>
      val outcome = run match
        case TestRun.Throws(err, duration, map) =>
          val exception: Exception = try err() catch case exc: Exception => exc
          if !map.isEmpty then inc2.include(runner.report, test.id, DebugInfo.Captures(map))
          Outcome.Throws(exception, duration)
    
        case TestRun.Returns(value, duration, map) =>
          try if predicate(value) then Outcome.Pass(duration) else
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

  def inspect[TestType: Type](expr: Expr[TestType], test: Expr[TestContext])(using Quotes): Expr[TestType] =
    import quotes.reflect.*

    val exprName: Text = expr.asTerm.pos match
      case pos: dtdu.SourcePosition => pos.lineContent.show.slice(pos.startColumn, pos.endColumn)
      case _                        => t"<unknown>"
    
    val debug: Expr[Debug[TestType]] =
      Expr.summon[Debug[TestType]].getOrElse('{ TextConversion.any })
    
    '{ $test.capture(Text(${Expr[String](exprName.s)}), $expr)(using $debug) }
