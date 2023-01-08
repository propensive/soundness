package probably2

import gossamer.*
import rudiments.*

import dotty.tools.dotc.util as dtdu
import scala.quoted.*

object ProbablyMacros:
  def assert[T: Type, R: Type]
            (test: Expr[Test[T]], pred: Expr[T => Boolean], runner: Expr[Runner[R]])
            (using Quotes)
            : Expr[Unit] =
    import quotes.reflect.*
    
    def extract(pred: Expr[T => Boolean]): Option[Expr[T]] = pred match
      case '{ (a: T) => $p(a): Boolean } =>
        p.asTerm match
          case Block(List(DefDef(af1, _, _, Some(ap))), Closure(Ident(af2), _)) if af1 == af2 =>
            ap match
              case Apply(Select(term, "=="), List(Ident(_))) => Some(term.asExprOf[T])
              case Apply(Select(Ident(_), "=="), List(term)) => Some(term.asExprOf[T])
              case _                                         => None
          case _ => None
    
    val opt = extract(pred)
    
    '{
      println(${Expr(opt.map(_.show).toString)})
      $runner.run($test)(ProbablyMacros.assertion)
    }
  
  def typed[T: Type, R: Type](test: Expr[Test[T]], runner: Expr[Runner[R]])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    '{()}

  def inspect[T: Type](expr: Expr[T], test: Expr[TestBody])(using Quotes): Expr[T] =
    import quotes.reflect.*

    val exprName: Text = expr.asTerm.pos match
      case pos: dtdu.SourcePosition => pos.lineContent.show.slice(pos.startColumn, pos.endColumn)
      case _                        => t"<unknown>"
    
    val debug: Expr[Debug[T]] = Expr.summon[Debug[T]].getOrElse('{ Debug.any })
    
    '{ $test.capture(Text(${Expr[String](exprName.s)}), $expr)(using $debug) }

  def assertion[T](run: Test.Run[T]): Test.Result = Test.Result.Pass(run.duration)