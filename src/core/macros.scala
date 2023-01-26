package probably

import gossamer.*
import rudiments.*
import deviation.*

import dotty.tools.dotc.util as dtdu
import scala.quoted.*

object ProbablyMacros:
  
  def succeed: Any => Boolean = (value: Any) => true
  
  def check[T: Type, R: Type]
           (test: Expr[Test[T]], pred: Expr[T => Boolean], runner: Expr[Runner[R]],
                inc: Expr[Inclusion[R, Outcome]])
           (using Quotes)
           : Expr[T] =
    import quotes.reflect.*
    
    def extract(pred: Expr[T => Boolean]): Option[Expr[Any]] = pred match
      case '{ (a: T) => $p(a): Boolean } => p.asTerm match
        case Block(List(DefDef(af1, _, _, Some(ap))), Closure(Ident(af2), _)) if af1 == af2 =>
          ap match
            case Apply(Select(term, "=="), List(Ident(_))) => Some(term.asExpr)
            case Apply(Select(Ident(_), "=="), List(term)) => Some(term.asExpr)
            case _                                         => None
        case _ => None
      
      case other =>
        None
    
    val opt = extract(pred)
    
    '{ assertion($runner, $test, $pred, $inc, _.get) }
  
  def assert[T: Type, R: Type]
            (test: Expr[Test[T]], pred: Expr[T => Boolean], runner: Expr[Runner[R]],
                 inc: Expr[Inclusion[R, Outcome]])
            (using Quotes)
            : Expr[Unit] =
    import quotes.reflect.*
    val chk: Expr[T] = check[T, R](test, pred, runner, inc)
    
    '{ assertion($runner, $test, $pred, $inc, _ => ()) }

  def assertion[T, R, S](runner: Runner[R], test: Test[T], pred: T => Boolean, inc: Inclusion[R, Outcome],
                             result: TestRun[T] => S): S =
    runner.run(test).pipe: run =>
      inc.include(runner.report, test.id, Outcome(run, pred))
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

  //def assertion[T](run: TestRun[T]): Test.Outcome = Test.Outcome.Pass(run.duration)
