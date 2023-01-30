package probably

import gossamer.*
import rudiments.*
import deviation.*
import chiaroscuro.*

import dotty.tools.dotc.util as dtdu
import scala.quoted.*

object ProbablyMacros:
  protected def general[T: Type, R: Type, S: Type]
           (test: Expr[Test[T]], pred: Expr[T => Boolean], runner: Expr[Runner[R]],
                inc: Expr[Inclusion[R, Outcome]], inc2: Expr[Inclusion[R, DebugInfo]],
                action: Expr[TestRun[T] => S])
           (using Quotes)
           : Expr[S] =
    import quotes.reflect.*
    
    val exp: Option[Expr[Any]] = pred.asTerm match
      case Inlined(_, _, Block(List(DefDef(a1, _, _, Some(e))), Closure(Ident(a2), _))) if a1 == a2 => e match
        case Apply(Select(Ident(_), "=="), List(term)) => Some(term.asExpr)
        case Apply(Select(term, "=="), List(Ident(_))) => Some(term.asExpr)
        case other                                     => None
      case _                                                                                        => None
    
    exp match
      case Some('{ $expr: t }) =>
        val debug: Expr[Debug[t | T]] = Expr.summon[Debug[t | T]].getOrElse('{ Debug.any })
        val comparable = Expr.summon[Comparable[t | T]].getOrElse('{Comparable.simplistic[t | T]})
        '{ assertion[t | T, T, R, S]($runner, $test, $pred, $action, $comparable, Some($expr), $inc, $inc2, $debug) }
      
      case _ =>
        '{ assertion[T, T, R, S]($runner, $test, $pred, $action, Comparable.nothing[T], None, $inc, $inc2, Debug.any) }
  
  def check[T: Type, R: Type]
           (test: Expr[Test[T]], pred: Expr[T => Boolean], runner: Expr[Runner[R]],
                inc: Expr[Inclusion[R, Outcome]], inc2: Expr[Inclusion[R, DebugInfo]])
           (using Quotes)
           : Expr[T] =
    import quotes.reflect.*
    general[T, R, T](test, pred, runner, inc, inc2, '{ (t: TestRun[T]) => t.get })
    
  def assert[T: Type, R: Type]
            (test: Expr[Test[T]], pred: Expr[T => Boolean], runner: Expr[Runner[R]],
                 inc: Expr[Inclusion[R, Outcome]], inc2: Expr[Inclusion[R, DebugInfo]])
            (using Quotes)
            : Expr[Unit] =
    import quotes.reflect.*
    general[T, R, Unit](test, pred, runner, inc, inc2, '{ (t: TestRun[T]) => () })

  def succeed: Any => Boolean = (value: Any) => true
  
  def assertion[T, T0 <: T, R, S](runner: Runner[R], test: Test[T0], pred: T0 => Boolean, result: TestRun[T0] => S,
                                      comparable: Comparable[T], exp: Option[T], inc: Inclusion[R, Outcome],
                                      inc2: Inclusion[R, DebugInfo], debug: Debug[T]): S =
    runner.run(test).pipe: run =>
      val outcome = run match
        case TestRun.Throws(err, duration, map) =>
          val exception: Exception =
            try
              err()
              ???
            catch case exc: Exception => exc
          if !map.isEmpty then inc2.include(runner.report, test.id, DebugInfo.Captures(map))
          Outcome.Throws(exception, duration)
    
        case TestRun.Returns(value, duration, map) =>
          try if pred(value) then Outcome.Pass(duration) else
            exp match
              case Some(exp) =>
                inc2.include(runner.report, test.id, DebugInfo.Compare(debug.show(exp), debug.show(value), comparable.compare(value, exp)))
              case None =>
                //inc2.include(runner.report, test.id, DebugInfo.Compare(summon[Comparable[Any]].compare(value, 1)))
            
            if !map.isEmpty then inc2.include(runner.report, test.id, DebugInfo.Captures(map))
            
            Outcome.Fail(duration)
          catch case err: Exception => Outcome.CheckThrows(err, duration)

      inc.include(runner.report, test.id, outcome)
      result(run)

  def typed[T: Type, R: Type](test: Expr[Test[T]], runner: Expr[Runner[R]])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    '{()}

  def inspect[T: Type](expr: Expr[T], test: Expr[TestContext])(using Quotes): Expr[T] =
    import quotes.reflect.*

    val exprName: Text = expr.asTerm.pos match
      case pos: dtdu.SourcePosition => pos.lineContent.show.slice(pos.startColumn, pos.endColumn)
      case _                        => t"<unknown>"
    
    val debug: Expr[Debug[T]] = Expr.summon[Debug[T]].getOrElse('{ Debug.any })
    
    '{ $test.capture(Text(${Expr[String](exprName.s)}), $expr)(using $debug) }
